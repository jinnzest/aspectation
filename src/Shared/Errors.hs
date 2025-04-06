module Shared.Errors
  ( Errors (Errors),
    errors,
    Error
      ( MultiRangedError,
        mrError,
        RangedError,
        rError,
        range,
        PositionedError,
        position,
        pError,
        Error
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
newtype Errors = Errors
  { errors :: [Error]
  }
  deriving stock (Eq, Generic)
  deriving (ToJSON) via Vanilla Errors

type Error :: Type
data Error
  = MultiRangedError {ranges :: [Range], mrError :: Text}
  | RangedError {range :: Range, rError :: Text}
  | PositionedError {position :: Position, pError :: Text}
  | Error Text
  deriving stock (Eq, Generic)
  deriving (ToJSON) via Vanilla Error

instance Show Error where
  show = showYaml

instance Show Errors where
  show = showYaml

instance ToText Errors where
  toText = fromString . showYaml
