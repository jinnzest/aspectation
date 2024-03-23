module Shared.Parser.Data
  ( Parser,
  )
where

import Data.Kind (Type)
import Data.Text (Text)
import Data.Void (Void)
import Text.Megaparsec (Parsec)

type Parser :: Type -> Type
type Parser = Parsec Void Text
