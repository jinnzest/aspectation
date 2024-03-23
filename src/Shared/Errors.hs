module Shared.Errors
  ( Errors (MkErrors),
    errors,
    Error
      ( MkMultiRangedError,
        mrError,
        MkRangedError,
        rError,
        range,
        MkPositionedError,
        position,
        pError,
        MkError
      ),
  )
where

import Data.Aeson (ToJSON)
import Data.Eq (Eq)
import Data.Function ((.))
import Data.Kind (Type)
import Data.String (fromString)
import Data.Text (Text)
import Deriving.Aeson (CustomJSON (CustomJSON), Generic)
import Deriving.Aeson.Stock (Vanilla)
import Shared.Location.Data (Position, Range)
import Shared.ShowYaml (showYaml)
import Text.Shakespeare.Text (ToText (toText))
import Text.Show (Show (show))

type Errors :: Type
newtype Errors = MkErrors
  { errors :: [Error]
  }
  deriving stock (Eq, Generic)
  deriving (ToJSON) via Vanilla Errors

type Error :: Type
data Error
  = MkMultiRangedError {ranges :: [Range], mrError :: Text}
  | MkRangedError {range :: Range, rError :: Text}
  | MkPositionedError {position :: Position, pError :: Text}
  | MkError Text
  deriving stock (Eq, Generic)
  deriving (ToJSON) via Vanilla Error

instance Show Error where
  show = showYaml

instance Show Errors where
  show = showYaml

instance ToText Errors where
  toText = fromString . showYaml
