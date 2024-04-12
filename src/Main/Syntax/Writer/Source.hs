module Main.Syntax.Writer.Source
  ( writeFunctions,
  )
where

import Control.Category (Category (id))
import Control.Monad.Writer (Monoid (mconcat))
import Data.Eq (Eq ((==)))
import Data.Foldable (Foldable (foldl))
import Data.Function (($))
import Data.List (map, reverse)
import Data.Maybe (maybe)
import Data.Text as T (Text, pack, unpack)
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
import Shared.Location.Data
  ( OcRanged
      ( OcRanged,
        cSpaces,
        oSpaces,
        ocItem
      ),
    Ranged (Ranged, rItem, rSpaces),
  )
import Text.Shakespeare.Text as ST (st)
import Text.Show (Show (show))

writeRangedWith :: (a -> Text) -> Ranged a -> Text
writeRangedWith builder Ranged {rItem, rSpaces} = [st|#{rSpaces}#{builder rItem}|]

writeSingleRanged :: Ranged Text -> Text
writeSingleRanged = writeRangedWith id

writeFunctionSignatureItem :: FunctionSignatureItem -> Text
writeFunctionSignatureItem (FunctionArgument arg) = writeExpression arg
writeFunctionSignatureItem (FunctionName name) = writeExpression name

writeSignature :: FunctionSignature -> Text
writeSignature (FunctionSignature constructs) = mconcat $ map writeFunctionSignatureItem constructs

writeExpression :: Expression -> Text
writeExpression (AlphaNumExpr ranged) = writeSingleRanged ranged
writeExpression (NonAlphaNumExpr ranged) = writeSingleRanged ranged
writeExpression (NumberExpr Ranged {rItem = Number {int, dec, exp}, rSpaces = rS}) =
  let decimalText = maybe "" (\d -> [st|.#{show d}|]) dec
      exponentText = maybe "" (\e -> [st|e#{show e}|]) exp
   in [st|#{rS}#{show int}#{decimalText}#{exponentText}|]
writeExpression (TextExpr Ranged {rItem, rSpaces}) =
  let escapedText = pack $ reverse $ foldl (\acc c -> if c == '"' then c : c : acc else c : acc) "" $ unpack rItem
   in [st|#{rSpaces}"#{escapedText}"|]
writeExpression (NestedExpr expressions) = mconcat $ map writeExpression expressions
writeExpression (HigherPriorityExpr OcRanged {ocItem = expressions, oSpaces = oS, cSpaces = cS}) =
  let joinedExpressions = mconcat $ map writeExpression expressions in [st|#{oS}(#{joinedExpressions}#{cS})|]

writeConstruct :: Ranged Function -> Text
writeConstruct Ranged {rItem = Function {fSignature, fBody = FunctionBody Ranged {rItem = expressions, rSpaces = bS}}, rSpaces} =
  let sigText = writeSignature fSignature
      bodyText = mconcat $ map writeExpression expressions
   in [st|#{rSpaces}#{sigText}#{bS}->#{bodyText}|]

writeFunctions :: [Ranged Function] -> Text
writeFunctions items = mconcat $ map writeConstruct items
