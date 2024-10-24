module Main.Syntax.Parsing.Parser
  ( Parser,
    syntaxParser,
  )
where

import Control.Applicative ((<$>))
import Control.Monad (Monad (return), void, when)
import Control.Monad.Except (MonadFail (fail))
import Data.Bool (Bool (False, True), (&&))
import Data.Char (Char)
import Data.Eq (Eq ((==)))
import Data.Function (($))
import Data.Functor (($>))
import Data.List (length, map)
import Data.List as L (all, foldr)
import Data.Maybe (Maybe (Just))
import Data.Ord (Ord ((>)))
import Data.Text as T (Text, pack)
import GHC.Num.Integer (Integer)
import Main.Syntax.Parsing.Nesting (mkNestedExprs)
import Main.Syntax.Parsing.Tree
  ( Expression
      ( AlphaNumExpr,
        HigherPriorityExpr,
        NestedExpr,
        NonAlphaNumExpr,
        NumberExpr,
        TextExpr
      ),
    Function (Function, fBody, fSignature),
    FunctionBody (FunctionBody),
    FunctionSignature (FunctionSignature),
    FunctionSignatureItem (FunctionArgument, FunctionName),
    Number (Number, dec, exp, int),
  )
import Shared.Conditional.Debug.Parse (dbg)
import Shared.Location.Data
  ( OcRanged (ocItem),
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
    <|> char '#'
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

idWord :: Parser Text
idWord = hidden (pack <$> some idChar) <?> "an identifier"

exprToFuncSig :: Expression -> [FunctionSignatureItem] -> [FunctionSignatureItem]
exprToFuncSig expr@(AlphaNumExpr _) acc = FunctionName expr : acc
exprToFuncSig expr@(NonAlphaNumExpr _) [] = [FunctionName expr]
exprToFuncSig expr@(NonAlphaNumExpr _) acc = FunctionName expr : acc
exprToFuncSig expr@(NumberExpr _) acc = FunctionArgument expr : acc
exprToFuncSig expr@(TextExpr _) acc = FunctionArgument expr : acc
exprToFuncSig expr@(HigherPriorityExpr _) acc = FunctionArgument expr : acc
exprToFuncSig (NestedExpr _) acc = acc -- there are no nested exprs in a func signature

withoutBrackets :: Expression -> Bool
withoutBrackets (HigherPriorityExpr _) = False
withoutBrackets _ = True

signedDecimal :: Parser Integer
signedDecimal = do
  signed <- optional $ char '-' <|> char '+'
  dec <- decimal
  return $ case signed of
    Just '-' -> -dec
    _ -> dec

numberExpr :: Indent -> Parser Expression
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

textExpr :: Indent -> Parser Expression
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

alNumExpr :: Indent -> Parser Expression
alNumExpr indent = dbg "alNumExpr" $ AlphaNumExpr <$> (hidden (ranged indent idWord) <?> "an identifier")

specBodyExpr :: Indent -> Parser Expression
specBodyExpr indent = NonAlphaNumExpr <$> (hidden (specBodyWord indent) <?> "an identifier")

specSigExpr :: Indent -> Parser Expression
specSigExpr indent = NonAlphaNumExpr <$> (hidden (specSigWord indent) <?> "an identifier")

bodyExprs :: Parser [Expression]
bodyExprs = many bodyExpr

higherPriorityBodyExpr :: Indent -> Parser Expression
higherPriorityBodyExpr indent =
  dbg "higherPriorityBodyExpr" $
    HigherPriorityExpr
      <$> ocRanged indent bodyExprs

alNumExprs :: Parser [Expression]
alNumExprs = many $ try $ alNumExpr Indented

isAlNumExpr :: Expression -> Bool
isAlNumExpr (AlphaNumExpr (Ranged {rItem = "_"})) = True
isAlNumExpr _ = False

higherPrioritySigExpr :: Indent -> Parser Expression
higherPrioritySigExpr indent =
  dbg "higherPrioritySigExpr" $ do
    rangedExprs <- ocRanged indent alNumExprs
    let exprs = ocItem rangedExprs
    if all isAlNumExpr exprs && (length exprs > 1)
      then fail "multiple underscore characters without non underscore words are not allowed"
      else return $ HigherPriorityExpr rangedExprs

funcSigExpr :: Indent -> Parser Expression
funcSigExpr indent =
  try (textExpr indent)
    <|> try (numberExpr indent)
    <|> try (specSigExpr indent)
    <|> try (alNumExpr indent)
    <|> try (higherPrioritySigExpr indent)

bodyExpr :: Parser Expression
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
          return $ mkNestedExprs arrowLine exprs
      )

funcSigItems :: Parser [FunctionSignatureItem]
funcSigItems =
  dbg "funcSigItems" $ do
    headExpr <- funcSigExpr NotIndented
    tailExprs <- many $ funcSigExpr Indented
    let exprs = headExpr : tailExprs
    if all withoutBrackets exprs
      then return $ FunctionName headExpr : map FunctionArgument tailExprs
      else return $ foldr exprToFuncSig [] exprs

funcSig :: Parser FunctionSignature
funcSig = dbg "funcSig" $ FunctionSignature <$> funcSigItems

func :: Parser Function
func = dbg "func" $ do
  fSignature <- funcSig
  fBody <- funcBody
  return Function {fSignature, fBody}

syntaxParser :: Parser ([Ranged Function], Text)
syntaxParser = do
  funcs <- dbg "funcs" $ some $ try $ ranged NotIndented func
  sp <- spaces
  eof
  return (funcs, sp)
