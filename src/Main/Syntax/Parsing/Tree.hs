module Main.Syntax.Parsing.Tree
  ( Function (Function, fBody, fSignature),
    SyntaxTree,
    Word (SingleWord, MultiWord),
    Number (Number, int, dec, exp),
    TokenExpr
      ( AlphaNumExpr,
        NonAlphaNumExpr,
        NumberExpr,
        TextExpr,
        NestedExpr,
        HigherPriorityExpr,
        SigHigherPriorityExpr
      ),
    ExprsBlock (ExprsBlock),
    FunctionSignatureItem (FunctionName, FunctionArgument),
    FunctionSignature (FunctionSignature, fsURL, fsItems),
    FunctionBody (FunctionBody),
  )
where

import Data.Eq (Eq)
import Data.Function ((.))
import Data.Hashable (Hashable)
import Data.Kind (Type)
import Data.List.NonEmpty (NonEmpty)
import Data.Maybe (Maybe)
import Data.Ord (Ord)
import Data.String (fromString)
import Data.Text (Text)
import Deriving.Aeson (CustomJSON (CustomJSON), Generic, ToJSON)
import Deriving.Aeson.Stock (Vanilla)
import GHC.Integer (Integer)
import Shared.Location.Data (OcRanged, Ranged)
import Shared.ShowYaml (showYaml)
import Text.Shakespeare.Text as ST (ToText (toText))
import Text.Show (Show, show)

type Word :: Type
data Word = SingleWord (Ranged Text) | MultiWord (OcRanged [Ranged Text])
  deriving stock (Eq, Generic)
  deriving (ToJSON) via Vanilla Word

instance Show Word where
  show = showYaml

type Number :: Type
data Number = Number
  { int :: Integer,
    dec :: Maybe Integer,
    exp :: Maybe Integer
  }
  deriving stock (Eq, Ord, Generic)
  deriving (ToJSON) via Vanilla Number

instance Hashable Number

type ExprsBlock :: Type
newtype ExprsBlock = ExprsBlock [TokenExpr]
  deriving stock (Eq, Ord, Generic)
  deriving (ToJSON) via Vanilla ExprsBlock

instance Hashable ExprsBlock

instance Show ExprsBlock where
  show = showYaml

instance ToText (NonEmpty ExprsBlock) where
  toText = fromString . showYaml

type TokenExpr :: Type
data TokenExpr
  = AlphaNumExpr (Ranged Text)
  | NonAlphaNumExpr (Ranged Text)
  | NumberExpr (Ranged Number)
  | TextExpr (Ranged Text)
  | HigherPriorityExpr (OcRanged [ExprsBlock])
  | SigHigherPriorityExpr (OcRanged [TokenExpr])
  | NestedExpr [ExprsBlock]
  deriving stock (Eq, Ord, Generic)
  deriving (ToJSON) via Vanilla TokenExpr

instance Hashable TokenExpr

type FunctionBody :: Type
newtype FunctionBody = FunctionBody (Ranged [ExprsBlock])
  deriving stock (Eq, Generic)
  deriving (ToJSON) via Vanilla FunctionBody

type FunctionSignatureItem :: Type
data FunctionSignatureItem = FunctionName TokenExpr | FunctionArgument TokenExpr
  deriving stock (Eq, Generic)
  deriving (ToJSON) via Vanilla FunctionSignatureItem

instance Hashable FunctionSignatureItem

type FunctionSignature :: Type
data FunctionSignature = FunctionSignature
  { fsItems :: [FunctionSignatureItem],
    fsURL :: Text
  }
  deriving stock (Eq, Generic)
  deriving (ToJSON) via Vanilla FunctionSignature

instance Hashable FunctionSignature

instance Show FunctionSignature where
  show = showYaml

instance Show FunctionBody where
  show = showYaml

instance Hashable FunctionBody

instance Show FunctionSignatureItem where
  show = showYaml

type Function :: Type
data Function = Function
  { fSignature :: FunctionSignature,
    fBody :: FunctionBody
  }
  deriving stock (Eq, Generic)
  deriving (ToJSON) via Vanilla Function

instance Hashable Function

instance ToText Function where
  toText = fromString . showYaml

instance Show Function where
  show = showYaml

instance ToText TokenExpr where
  toText = fromString . showYaml

instance Show TokenExpr where
  show = showYaml

instance ToText [TokenExpr] where
  toText = fromString . showYaml

instance ToText [[TokenExpr]] where
  toText = fromString . showYaml

instance ToText ExprsBlock where
  toText = fromString . showYaml

instance ToText [ExprsBlock] where
  toText = fromString . showYaml

instance ToText (Ranged Function) where
  toText = fromString . showYaml

type SyntaxTree :: Type
type SyntaxTree = [Ranged Function]

instance ToText SyntaxTree where
  toText = fromString . showYaml

instance ToText FunctionSignatureItem where
  toText = fromString . showYaml

instance ToText FunctionSignature where
  toText = fromString . showYaml
