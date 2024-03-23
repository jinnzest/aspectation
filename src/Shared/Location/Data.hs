module Shared.Location.Data
  ( Range (Range, from, to),
    Ranged (Ranged, rItem, range, rSpaces),
    OcRanged (OcRanged, ocItem, ocRange, oSpaces, cSpaces),
    Position (Position, line, column),
  )
where

import Data.Aeson ((.=))
import Data.Aeson.Types (ToJSON (toJSON), object)
import Data.Eq (Eq)
import Data.Int (Int)
import Data.Kind (Type)
import Data.Ord (Ord)
import Data.Text (Text)
import Deriving.Aeson (CustomJSON (CustomJSON), Generic)
import Deriving.Aeson.Stock (Vanilla)
import Shared.Conditional.Debug.Loc (locEnabled)
import Shared.ShowYaml (showYaml)
import Text.Shakespeare.Text (ToText (toText), st)
import Text.Show (Show (show))

type Position :: Type
data Position = Position
  { line :: Int,
    column :: Int
  }
  deriving stock (Eq, Ord, Generic)
  deriving (ToJSON) via Vanilla Position

type Range :: Type
data Range = Range
  { from :: Position,
    to :: Position
  }
  deriving stock (Eq, Ord, Generic)
  deriving (ToJSON) via Vanilla Range

type Ranged :: Type -> Type
data Ranged a = Ranged
  { rSpaces :: Text,
    range :: Range,
    rItem :: a
  }
  deriving stock (Eq, Ord, Generic)

instance ToJSON a => ToJSON (Ranged a) where
  toJSON Ranged {rItem = i, range = r} =
    if locEnabled
      then object ["rItem" .= i, "range" .= r]
      else object ["rItem" .= i]

type OcRanged :: Type -> Type
data OcRanged a = OcRanged
  { oSpaces :: Text,
    ocRange :: Range,
    ocItem :: a,
    cSpaces :: Text
  }
  deriving stock (Eq, Ord, Generic)

instance ToJSON a => ToJSON (OcRanged a) where
  toJSON OcRanged {ocItem = i, ocRange = r, oSpaces = os, cSpaces = cs} =
    if locEnabled
      then object ["ocItem" .= i, "ocRange" .= r, "oSpaces" .= os, "cSpaces" .= cs]
      else object ["ocItem" .= i]

instance Show Position where
  show = showYaml

instance Show Range where
  show = showYaml

instance ToJSON a => Show (Ranged a) where
  show = showYaml

instance ToJSON a => Show (OcRanged a) where
  show = showYaml

instance ToText Position where
  toText Position {line = l, column = c} = toText [st|#{l}:#{c}|]

instance ToText Range where
  toText Range {from = f, to = t} = toText [st|#{f} - #{t}|]

instance ToText a => ToText (Ranged a) where
  toText Ranged {rItem = i, range = r} = toText [st|#{r} #{i}|]

instance ToText a => ToText (OcRanged a) where
  toText OcRanged {ocItem = i, ocRange = r} = toText [st|#{r} #{i}|]
