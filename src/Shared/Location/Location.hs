module Shared.Location.Location
  ( ranged,
    spaces,
    keychar,
    keyword,
    ocRanged,
    Indent (Indented, NotIndented),
  )
where

import Control.Monad (Monad (return))
import Data.Char (Char)
import Data.Eq (Eq)
import Data.Function (($))
import Data.Functor (($>), (<$>))
import Data.Kind (Type)
import Data.Ord (Ordering (EQ, GT))
import Data.Text (Text, pack, singleton)
import Shared.Conditional.Debug.Parse (dbg)
import Shared.Location.Data
  ( OcRanged
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
import Shared.Parser.Data (Parser)
import Text.Megaparsec (SourcePos (sourceColumn), getSourcePos, many, (<|>))
import Text.Megaparsec.Char (char, string)
import Text.Megaparsec.Char.Lexer as TL (indentGuard)
import Text.Megaparsec.Pos (SourcePos (sourceLine), pos1, unPos)

type Indent :: Type
data Indent = Indented | NotIndented deriving stock (Eq)

position :: Parser Position
position = do
  pos <- getSourcePos
  return
    Position
      { line = unPos $ sourceLine pos,
        column = unPos $ sourceColumn pos
      }

spaces :: Parser Text
spaces =
  dbg "spaces" $
    pack
      <$> many
        ( char '\n' <|> char '\r' <|> char '\t' <|> char ' '
        )

indentGuard :: Indent -> Parser ()
indentGuard Indented = TL.indentGuard (return ()) GT pos1 $> ()
indentGuard NotIndented = TL.indentGuard (return ()) EQ pos1 $> ()

ranged :: Indent -> Parser a -> Parser (Ranged a)
ranged indented parser = do
  rSpaces <- spaces
  Shared.Location.Location.indentGuard indented
  from <- position
  rItem <- parser
  to <- position
  let range = Range {from, to}
  return $ Ranged {rSpaces, rItem, range}

keychar :: Indent -> Char -> Parser (Ranged Text)
keychar indented kc = ranged indented $ singleton <$> char kc

keyword :: Indent -> Text -> Parser (Ranged Text)
keyword indented word = ranged indented $ string word

ocRanged :: Indent -> Parser a -> Parser (OcRanged a)
ocRanged indented parser = do
  oRanged <- keychar indented '('
  ocItem <- parser
  cRanged <- keychar Indented ')'
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
