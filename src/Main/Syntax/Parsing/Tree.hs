module Main.Syntax.Parsing.Tree
  ( Function (Function, fBody, fSignature),
    SyntaxTree,
    Word (SingleWord, MultiWord),
    Number (Number, int, dec, exp),
    Expression
      ( AlphaNumExpr,
        NonAlphaNumExpr,
        NumberExpr,
        TextExpr,
        NestedExpr,
        HigherPriorityExpr
      ),
    FunctionSignatureItem (FunctionName, FunctionArgument),
    FunctionSignature (FunctionSignature),
    FunctionBody (FunctionBody),
  )
where

import Data.Eq (Eq)
import Data.Function ((.))
import Data.Kind (Type)
import Data.Maybe (Maybe)
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
  deriving stock (Eq, Generic)
  deriving (ToJSON) via Vanilla Number

type Expression :: Type
data Expression
  = AlphaNumExpr (Ranged Text)
  | NonAlphaNumExpr (Ranged Text)
  | NumberExpr (Ranged Number)
  | TextExpr (Ranged Text)
  | NestedExpr [Expression]
  | HigherPriorityExpr (OcRanged [Expression])
  deriving stock (Eq, Generic)
  deriving (ToJSON) via Vanilla Expression

type FunctionBody :: Type
newtype FunctionBody = FunctionBody (Ranged [Expression])
  deriving stock (Eq, Generic)
  deriving (ToJSON) via Vanilla FunctionBody

type FunctionSignatureItem :: Type
data FunctionSignatureItem = FunctionName [Expression] | FunctionArgument Expression
  deriving stock (Eq, Generic)
  deriving (ToJSON) via Vanilla FunctionSignatureItem

type FunctionSignature :: Type
newtype FunctionSignature = FunctionSignature [FunctionSignatureItem]
  deriving stock (Eq, Generic)
  deriving (ToJSON) via Vanilla FunctionSignature

instance Show FunctionSignature where
  show = showYaml

instance Show FunctionBody where
  show = showYaml

instance Show FunctionSignatureItem where
  show = showYaml

type Function :: Type
data Function = Function
  { fSignature :: FunctionSignature,
    fBody :: FunctionBody
  }
  deriving stock (Eq, Generic)
  deriving (ToJSON) via Vanilla Function

instance ToText Function where
  toText = fromString . showYaml

instance Show Function where
  show = showYaml

instance ToText Expression where
  toText = fromString . showYaml

instance Show Expression where
  show = showYaml

instance ToText [Expression] where
  toText = fromString . showYaml

instance ToText (Ranged Function) where
  toText = fromString . showYaml

type SyntaxTree :: Type
type SyntaxTree = [Ranged Function]

instance ToText SyntaxTree where
  toText = fromString . showYaml
