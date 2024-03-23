module Main.Syntax.Writer.Ranged
  ( writeFunctions,
  )
where

import Data.Bool (Bool (False, True), not)
import Data.Foldable (Foldable (foldr))
import Data.Function (($), (.))
import Data.Int (Int)
import Data.List as L (filter, length, map)
import Data.Maybe (Maybe (Just, Nothing))
import Data.Monoid (Monoid (mconcat))
import Data.Text as T (Text, concat)
import GHC.Num (Num ((+), (-)))
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
        ocItem,
        ocRange
      ),
    Ranged (rItem, rSpaces, range),
  )
import Shared.Text.Utils (hasNL, indent, nL)
import Text.Shakespeare.Text (st)
import Text.Show (Show (show))

nlT :: Text
nlT = [st|new line|]

nlText :: Int -> Text -> Text
nlText ind spaces =
  let text = if hasNL spaces then [st|#{nL}#{indent ind}#{nlT}|] else ""
   in [st|#{text}#{nL}#{indent ind}|]

writeRanged :: Text -> (Int -> a -> Text) -> Ranged a -> Int -> Text
writeRanged label builder ranged ind =
  let nextInd = ind + 1
      built = builder nextInd $ rItem ranged
      nlTT = nlText ind (rSpaces ranged)
   in [st|#{nlTT}#{range ranged} #{label}#{built}|]

writeOcRanged :: Text -> (Int -> a -> Text) -> OcRanged a -> Int -> Text
writeOcRanged label builder ranged ind =
  let nextInd = ind + 1
      built = builder nextInd $ ocItem ranged
      onlTT = nlText ind (oSpaces ranged)
      cnlTT = if hasNL (cSpaces ranged) then [st|#{nL}#{indent nextInd}#{nlT}|] else ""
   in [st|#{onlTT}#{ocRange ranged} #{label}#{built}#{cnlTT}|]

writeSingleWord :: Int -> Text -> Text
writeSingleWord ind word = [st|#{nL}#{indent ind}#{word}|]

writeSignatureItem :: Int -> Int -> FunctionSignatureItem -> Int -> (Int, Int, Text)
writeSignatureItem nameNum argNum (FunctionName expressions) ind =
  ( nameNum - 1,
    argNum,
    let functionNameText = T.concat $ map (\e -> writeExpression e (ind + 1)) expressions
     in [st|#{nL}#{indent ind}name #{nameNum}#{functionNameText}|]
  )
writeSignatureItem nameNum argNum (FunctionArgument expression) ind =
  ( nameNum,
    argNum - 1,
    let nextInd = ind + 1
        expressionText = case expression of
          (HigherPriorityExpr OcRanged {ocItem = subExpressions}) ->
            case subExpressions of
              [] -> [st|#{nL}#{indent nextInd}empty argument|]
              expressions ->
                let expressionsText = T.concat $ map (`writeExpression` (nextInd + 1)) expressions
                 in [st|#{nL}#{indent nextInd}argument in brackets #{argNum}#{expressionsText}|]
          expr -> writeExpression expr nextInd
     in [st|#{nL}#{indent ind}argument #{argNum}#{expressionText}|]
  )

isName :: FunctionSignatureItem -> Bool
isName (FunctionName _) = True
isName _ = False

isArg :: FunctionSignatureItem -> Bool
isArg = not . isName

itemsCount :: (a -> Bool) -> [a] -> Int
itemsCount filterBy list = length $ filter filterBy list

sigItemAggregator :: FunctionSignatureItem -> (Int, Int, Int, [Text]) -> (Int, Int, Int, [Text])
sigItemAggregator i (n, a, ind, texts) =
  let (n2, a2, text) = writeSignatureItem n a i ind in (n2, a2, ind, text : texts)

writeFunctionSignatureItems :: Int -> [FunctionSignatureItem] -> Text
writeFunctionSignatureItems ind constructs =
  let nameNum = itemsCount isName constructs
      argNum = itemsCount isArg constructs
   in mconcat $ (\(_, _, _, r) -> r) $ foldr sigItemAggregator (nameNum, argNum, ind, []) constructs

writeSignature :: FunctionSignature -> Int -> Text
writeSignature (FunctionSignature items) ind =
  let nextInd = ind + 1
      itemsText = writeFunctionSignatureItems nextInd items
   in [st|#{nL}#{indent ind}function signature#{itemsText}|]

writeExpression :: Expression -> Int -> Text
writeExpression (AlphaNumExpr ranged) = writeRanged "alpha numberic word" writeSingleWord ranged
writeExpression (NonAlphaNumExpr ranged) = writeRanged "non alpha numberic word" writeSingleWord ranged
writeExpression (NumberExpr ranged) =
  writeRanged
    "number"
    ( \ind Number {int, dec, exp} ->
        let nextInd = ind + 1
            decimalText = case dec of
              Nothing -> ""
              Just d -> [st|#{nL}#{indent ind}decimal#{nL}#{indent nextInd}#{show d}|]
            exponentText = case exp of
              Nothing -> ""
              Just e -> [st|#{nL}#{indent ind}exponent#{nL}#{indent nextInd}#{show e}|]
         in [st|#{nL}#{indent ind}integer#{nL}#{indent nextInd}#{show int}#{decimalText}#{exponentText}|]
    )
    ranged
writeExpression (TextExpr ranged) = writeRanged "text" writeSingleWord ranged
writeExpression (NestedExpr expressions) = \ind ->
  let expressionsText = mconcat $ map (\e -> writeExpression e (ind + 2)) expressions
   in [st|#{nL}#{indent ind}nested expressions#{expressionsText}|]
writeExpression (HigherPriorityExpr ranged) =
  writeOcRanged "higher priority expressions" (\nextInd expr -> mconcat $ map (`writeExpression` nextInd) expr) ranged

writeBody :: Ranged [Expression] -> Int -> Text
writeBody =
  writeRanged
    "function body"
    (\nextInd expression -> let expressionTexts = map (`writeExpression` nextInd) expression in mconcat expressionTexts)

writeFunction :: Ranged Function -> Int -> Text
writeFunction ranged =
  writeRanged
    ( case rItem ranged of
        Function {} -> "function"
    )
    ( \nextInd c -> case c of
        Function {fSignature = signature, fBody = FunctionBody body} ->
          let signatureText = writeSignature signature nextInd
              bodyText = writeBody body nextInd
           in [st|#{signatureText}#{bodyText}|]
    )
    ranged

writeFunctions :: [Ranged Function] -> Text
writeFunctions = mconcat . map (`writeFunction` 0)
