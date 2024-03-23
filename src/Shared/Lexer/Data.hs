module Shared.Lexer.Data
  ( Lexer,
  )
where

import Data.Kind (Type)
import Data.Text (Text)
import Data.Void (Void)
import Text.Megaparsec (Parsec)

type Lexer :: Type -> Type
type Lexer = Parsec Void Text
