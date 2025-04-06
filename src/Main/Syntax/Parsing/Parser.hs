module Main.Syntax.Parsing.Parser
  ( Parser,
    syntaxParsing,
  )
where

import Control.Applicative ((<$>))
import Control.Monad (Monad (return), void, when)
import Control.Monad.Except (MonadFail (fail))
import Data.Bool (Bool (False, True), (&&))
import Data.Char (Char)
import Data.Eq (Eq ((==)))
import Data.Function (const, ($))
import Data.Functor (($>))
import Data.Kind (Type)
import Data.List as L (all, length, map)
import Data.Maybe (Maybe (Just, Nothing))
import Data.Ord (Ord ((>)))
import Data.Text as T (Text, pack)
import GHC.Num.Integer (Integer)
import Main.Syntax.Parsing.Layouting (mkLayouts)
import Main.Syntax.Parsing.Tree
  ( ExprsBlock,
    Function (Function, fBody, fSignature),
    FunctionBody (FunctionBody),
    FunctionSignature (FunctionSignature, fsItems, fsURL),
    FunctionSignatureItem (FunctionArgument, FunctionName),
    Number (Number, dec, exp, int),
    TokenExpr
      ( AlphaNumExpr,
        HigherPriorityExpr,
        NestedExpr,
        NonAlphaNumExpr,
        NumberExpr,
        SigHigherPriorityExpr,
        TextExpr
      ),
  )
import Shared.Conditional.Debug.Parse (dbg)
import Shared.Location.Data
  ( OcRanged (OcRanged, cSpaces, oSpaces, ocItem, ocRange),
    Position (line),
    Range (from),
    Ranged (Ranged, rItem, range),
  )
import Shared.Location.Location
  ( Indent (Indented, NotIndented),
    keyword,
    ocRanged,
    ranged,
    spaces,
  )
import Shared.Parser.Data (Parser)
import System.Directory.Internal.Prelude (Int, error)
import Text.Megaparsec
  ( MonadParsec (notFollowedBy),
    anySingleBut,
    eof,
    hidden,
    many,
    optional,
    some,
    try,
    (<?>),
    (<|>),
  )
import Text.Megaparsec.Char (alphaNumChar, char, letterChar, string)
import Text.Megaparsec.Char.Lexer (decimal)

specChar :: Parser Char
specChar =
  char '+'
    <|> char '-'
    <|> char '='
    <|> char '*'
    <|> char '/'
    <|> char '^'
    <|> char '&'
    <|> char ','
    <|> char '<'
    <|> char '>'
    <|> char '|'
    <|> char '?'
    <|> char '['
    <|> char ']'
    <|> char '{'
    <|> char '}'
    <|> char '\''
    <|> char '%'
    <|> char ':'
    <|> char ';'
    <|> char '.'
    <|> char '~'
    <|> char '!'
    <|> char '@'
    <|> char '$'
    <|> char '`'
    <|> char '\\'

underscoreChar :: Parser Char
underscoreChar = char '_'

specBodyWord :: Indent -> Parser (Ranged Text)
specBodyWord indent = ranged indent (pack <$> some specChar)

specSigWord :: Indent -> Parser (Ranged Text)
specSigWord indent =
  ranged
    indent
    ( pack <$> do
        headChar <- specChar
        when (headChar == '-') $ notFollowedBy $ char '>'
        tailChars <- many specChar
        return $ headChar : tailChars
    )

idChar :: Parser Char
idChar = try alphaNumChar <|> try underscoreChar

idText :: Parser Text
idText = hidden (pack <$> some idChar) <?> "an identifier"

signedDecimal :: Parser Integer
signedDecimal = do
  signed <- optional $ char '-' <|> char '+'
  dec <- decimal
  return $ case signed of
    Just '-' -> -dec
    _ -> dec

numberExpr :: Indent -> Parser TokenExpr
numberExpr indent =
  dbg "numberExpr" $
    NumberExpr
      <$> ranged
        indent
        ( ( do
              int <- hidden signedDecimal
              dec <- optional $ try $ do
                void $ hidden $ char '.'
                hidden decimal
              exp <- optional $ try $ do
                void $ hidden (char 'e' <|> char 'E')
                hidden signedDecimal
              notFollowedBy letterChar
              return Number {int, dec, exp}
          )
            <?> "a number"
        )

escapeQuotes :: Parser Char
escapeQuotes = string "\"\"" $> '\"'

textExpr :: Indent -> Parser TokenExpr
textExpr indent =
  TextExpr
    <$> ranged
      indent
      ( ( do
            void $ char '"'
            txt <- many (try (anySingleBut '\"') <|> try escapeQuotes)
            void $ char '"'
            return $ pack txt
        )
          <?> "a text"
      )

alNumExpr :: Indent -> Parser TokenExpr
alNumExpr indent = dbg "alNumExpr" $ AlphaNumExpr <$> (hidden (ranged indent idText) <?> "an identifier")

specBodyExpr :: Indent -> Parser TokenExpr
specBodyExpr indent = NonAlphaNumExpr <$> (hidden (specBodyWord indent) <?> "an identifier")

specSigExpr :: Indent -> Parser TokenExpr
specSigExpr indent = NonAlphaNumExpr <$> (hidden (specSigWord indent) <?> "an identifier")

hashSigWord :: Parser (Ranged Text)
hashSigWord =
  ranged
    NotIndented
    ( pack <$> do
        headChar <- char '#'
        return [headChar]
    )

hashSigExpr :: Parser TokenExpr
hashSigExpr = NonAlphaNumExpr <$> (hidden hashSigWord <?> "a hash")

bodyExprs :: Int -> Parser [ExprsBlock]
bodyExprs baseLine = do
  exprs <- some bodyExpr
  return $ mkLayouts baseLine exprs

higherPriorityBodyExpr :: Indent -> Parser TokenExpr
higherPriorityBodyExpr indent = dbg "higherPriorityBodyExpr" $ do
  rangedExprs <- ocRanged indent bodyExprs
  let exprs = ocItem rangedExprs
  return $ HigherPriorityExpr $ OcRanged {ocItem = exprs, ocRange = ocRange rangedExprs, oSpaces = oSpaces rangedExprs, cSpaces = cSpaces rangedExprs}

alNumExprs :: Parser [TokenExpr]
alNumExprs = many $ try $ alNumExpr Indented

isAlNumExpr :: TokenExpr -> Bool
isAlNumExpr (AlphaNumExpr (Ranged {rItem = "_"})) = True
isAlNumExpr _ = False

higherPrioritySigExpr :: Indent -> Parser TokenExpr
higherPrioritySigExpr indent =
  dbg "higherPrioritySigExpr" $ do
    rangedExprs <- ocRanged indent (const alNumExprs)
    let exprs = ocItem rangedExprs
    if all isAlNumExpr exprs && (length exprs > 1)
      then fail "multiple underscore characters without non underscore words are not allowed"
      else return $ SigHigherPriorityExpr OcRanged {ocItem = exprs, ocRange = ocRange rangedExprs, oSpaces = oSpaces rangedExprs, cSpaces = cSpaces rangedExprs}

funcSigExpr :: Indent -> Parser TokenExpr
funcSigExpr indent =
  try (textExpr indent)
    <|> try (numberExpr indent)
    <|> try (specSigExpr indent)
    <|> try (alNumExpr indent)
    <|> try (higherPrioritySigExpr indent)

bodyExpr :: Parser TokenExpr
bodyExpr =
  dbg "expr" $
    try (higherPriorityBodyExpr Indented)
      <|> try (numberExpr Indented)
      <|> try (alNumExpr Indented)
      <|> try (specBodyExpr Indented)
      <|> try (textExpr Indented)

funcBody :: Parser FunctionBody
funcBody =
  FunctionBody
    <$> ranged
      Indented
      ( do
          arrowItem <- keyword Indented "->"
          exprs <- some bodyExpr
          let arrowLine = line $ from $ range arrowItem
          return $ mkLayouts arrowLine exprs
      )

exprToFuncSig :: TokenExpr -> FunctionSignatureItem
exprToFuncSig expr@(AlphaNumExpr _) = FunctionName expr
exprToFuncSig expr@(NonAlphaNumExpr _) = FunctionName expr
exprToFuncSig expr@(NumberExpr _) = FunctionName expr
exprToFuncSig (TextExpr _) = error "text exprs in a complex func signature are not supported"
exprToFuncSig (HigherPriorityExpr _) = error "HigherPriorityExpr in a complex func signature is not supported"
exprToFuncSig expr@(SigHigherPriorityExpr _) = FunctionArgument expr
exprToFuncSig (NestedExpr _) = error "nested exprs in a complex func signature are not supported"

simpleFuncSigExpr :: Parser TokenExpr
simpleFuncSigExpr =
  try (textExpr Indented)
    <|> try (numberExpr Indented)
    <|> try (specSigExpr Indented)
    <|> try (alNumExpr Indented)

complexFuncSigExpr :: Parser TokenExpr
complexFuncSigExpr =
  try (higherPrioritySigExpr Indented)
    <|> try (numberExpr Indented)
    <|> try (specSigExpr Indented)
    <|> try (alNumExpr Indented)

funcSigExprs :: Parser TokenExpr -> Parser [TokenExpr]
funcSigExprs parser = do
  expr <- optional parser
  case expr of
    Nothing -> return []
    Just e -> do
      other <- funcSigExprs parser
      return $ e : other

type ParsingSigResults :: Type
data ParsingSigResults = Common [TokenExpr] | Simple [TokenExpr] | Complex [TokenExpr]

funcSigExprsRes :: Indent -> Parser ParsingSigResults
funcSigExprsRes indent = do
  expr <- optional $ funcSigExpr indent
  case expr of
    Nothing -> return $ Common []
    Just e -> case e of
      (SigHigherPriorityExpr _) -> do
        other <- funcSigExprs complexFuncSigExpr
        return $ Complex $ e : other
      (TextExpr _) -> do
        other <- funcSigExprs simpleFuncSigExpr
        return $ Simple $ e : other
      _ -> do
        other <- funcSigExprsRes Indented
        case other of
          Common common -> return $ Common $ e : common
          Simple simple -> return $ Simple $ e : simple
          Complex complex -> return $ Complex $ e : complex

funcSigResults :: Parser ParsingSigResults
funcSigResults = do
  hash <- optional hashSigExpr
  case hash of
    Nothing -> funcSigExprsRes NotIndented
    Just h -> do
      res <- funcSigExprsRes Indented
      case res of
        Common common -> return $ Common $ h : common
        Simple simple -> return $ Simple $ h : simple
        Complex complex -> return $ Complex $ h : complex

unparsableFirstExprError :: Parser [FunctionSignatureItem]
unparsableFirstExprError = do
  _ <- funcSigExpr NotIndented
  return []

funcSigItems :: Parser [FunctionSignatureItem]
funcSigItems =
  dbg "funcSigItems" $ do
    results <- funcSigResults
    case results of
      Common (h : t) -> return $ FunctionName h : map FunctionArgument t
      Simple (h : t) -> return $ FunctionName h : map FunctionArgument t
      Complex complex -> return $ map exprToFuncSig complex
      _ -> unparsableFirstExprError

funcSig :: Text -> Parser FunctionSignature
funcSig fsURL = dbg "funcSig" $ do
  fsItems <- funcSigItems
  return FunctionSignature {fsItems, fsURL}

func :: Text -> Parser Function
func fURL = dbg "func" $ do
  fSignature <- funcSig fURL
  fBody <- funcBody
  return Function {fSignature, fBody}

syntaxParsing :: Text -> Parser ([Ranged Function], Text)
syntaxParsing fileURL = do
  funcs <- dbg "funcs" $ some $ try $ ranged NotIndented (func fileURL)
  sp <- spaces
  eof
  return (funcs, sp)
