module Shared.Location.Location
  ( ranged,
    spaces,
    keyword,
    ocRanged,
    bracketsRanged,
    Indent (Indented, NotIndented),
  )
where

import Control.Monad (return, void)
import Data.Aeson (ToJSON)
import Data.Char (Char)
import Data.Eq (Eq)
import Data.Function (($))
import Data.Functor (($>), (<$>))
import Data.Int (Int)
import Data.Kind (Type)
import Data.Ord (Ordering (EQ, GT))
import Data.Text (Text, pack)
import Shared.Conditional.Debug.Parse (dbg)
import Shared.Lexer.Data (Lexer)
import Shared.Location.Data
  ( BracketsKind (CurlyBrackets, RoundBrackets, SquareBrackets),
    OcRanged
      ( OcRanged,
        cSpaces,
        oSpaces,
        ocItem,
        ocRange
      ),
    Position (Position, column, line),
    Range (Range, from, to),
    Ranged (Ranged, rItem, rSpaces, range),
  )
import Text.Megaparsec (SourcePos (sourceColumn), getSourcePos, many, (<?>), (<|>))
import Text.Megaparsec.Char (char, string)
import Text.Megaparsec.Char.Lexer as TL (indentGuard)
import Text.Megaparsec.Pos (SourcePos (sourceLine), pos1, unPos)

type Indent :: Type
data Indent = Indented | NotIndented deriving stock (Eq)

position :: Lexer Position
position = do
  pos <- getSourcePos
  return
    Position
      { line = unPos $ sourceLine pos,
        column = unPos $ sourceColumn pos
      }

spaces :: Lexer Text
spaces =
  dbg "spaces" $
    pack
      <$> many
        (char '\n' <|> char '\r' <|> char '\t' <|> char ' ')

indentGuard :: Indent -> Lexer ()
indentGuard Indented = TL.indentGuard (return ()) GT pos1 $> ()
indentGuard NotIndented = TL.indentGuard (return ()) EQ pos1 $> ()

ranged :: Indent -> Lexer a -> Lexer (Ranged a)
ranged indented parser = do
  rSpaces <- spaces
  Shared.Location.Location.indentGuard indented
  from <- position
  rItem <- parser
  to <- position
  let range = Range {from, to}
  return $ Ranged {rSpaces, rItem, range}

keyword :: Indent -> Text -> Lexer (Ranged Text)
keyword indented word = ranged indented $ string word

ocRanged :: (ToJSON a) => Lexer (Ranged ()) -> Lexer (Ranged ()) -> (Int -> Lexer a) -> Lexer (OcRanged a)
ocRanged openRangeParser closeRangeParse parser = dbg "ocRanged" $ do
  oRanged <- openRangeParser
  let bracketLine = line $ from $ range oRanged
  ocItem <- parser bracketLine
  cRanged <- closeRangeParse
  let ocRange =
        Range
          { from = from $ range oRanged,
            to = to $ range cRanged
          }
  return $
    OcRanged
      { oSpaces = rSpaces oRanged,
        cSpaces = rSpaces cRanged,
        ocItem,
        ocRange
      }

keychar :: Indent -> Char -> Lexer (Ranged ())
keychar indented kc = ranged indented (void (char kc))

openBracket :: BracketsKind -> Indent -> Lexer (Ranged ())
openBracket RoundBrackets ind = keychar ind '(' <?> "open bracket"
openBracket SquareBrackets ind = keychar ind '[' <?> "open bracket"
openBracket CurlyBrackets ind = keychar ind '{' <?> "open bracket"

closeBracket :: BracketsKind -> Lexer (Ranged ())
closeBracket RoundBrackets = keychar Indented ')' <?> "close bracket"
closeBracket SquareBrackets = keychar Indented ']' <?> "close bracket"
closeBracket CurlyBrackets = keychar Indented '}' <?> "close bracket"

bracketsRanged :: (ToJSON a) => BracketsKind -> Indent -> (Int -> Lexer a) -> Lexer (OcRanged a)
bracketsRanged bracketsKind ind = ocRanged (openBracket bracketsKind ind) (closeBracket bracketsKind)
