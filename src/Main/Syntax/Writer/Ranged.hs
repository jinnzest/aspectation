module Main.Syntax.Writer.Ranged
  ( writeFunctions,
  )
where

import Data.Bool (Bool (False, True), not)
import Data.Foldable (Foldable (foldr, null))
import Data.Function (($), (.))
import Data.Int (Int)
import Data.List (head, last)
import Data.List as L (filter, length, map)
import Data.Maybe (Maybe (Just, Nothing))
import Data.Monoid (Monoid (mconcat))
import Data.Text as T (Text, concat)
import GHC.Num (Num ((+), (-)))
import Main.Syntax.Parsing.Shared (getRange)
import Main.Syntax.Parsing.Tree
  ( ExprsBlock (ExprsBlock),
    Function (Function, fBody, fSignature),
    FunctionBody (FunctionBody),
    FunctionSignature (FunctionSignature, fsItems),
    FunctionSignatureItem (FunctionArgument, FunctionName),
    Number (Number, dec, exp, int),
    TokenExpr
      ( AlphaNumExpr,
        HigherPriorityExpr,
        NestedExpr,
        NonAlphaNumExpr,
        NumberExpr,
        SigHigherPriorityExpr,
        TextExpr
      ),
  )
import Shared.Location.Data
  ( OcRanged
      ( OcRanged,
        ocItem,
        ocRange
      ),
    Range (Range, from, to),
    Ranged (rItem, range),
  )
import Shared.Text.Utils (indent, nL)
import Text.Shakespeare.Text (ToText (toText), st)
import Text.Show (Show (show))

nlText :: Int -> Text
nlText ind = [st|#{nL}#{indent ind}|]

writeRanged :: Text -> (Int -> a -> Text) -> Ranged a -> Int -> Text
writeRanged label builder ranged ind =
  let nextInd = ind + 1
      built = builder nextInd $ rItem ranged
      nlTT = nlText ind
   in [st|#{nlTT}#{range ranged} #{label}#{built}|]

writeOcRanged :: Text -> (Int -> a -> Text) -> OcRanged a -> Int -> Text
writeOcRanged label builder ranged ind =
  let nextInd = ind + 1
      built = builder nextInd $ ocItem ranged
      onlTT = nlText ind
   in -- cnlTT = if hasNL (cSpaces ranged) then [st|#{nL}#{indent nextInd}|] else ""
      [st|#{onlTT}#{ocRange ranged} #{label}#{built}|]

writeSingleWord :: Int -> Text -> Text
writeSingleWord ind word = [st|#{nL}#{indent ind}#{word}|]

writeExprsBlock :: Int -> ExprsBlock -> Text
writeExprsBlock ind (ExprsBlock tokens) =
  let nextInd = ind + 1
      exprsText = mconcat $ map (`writeExpr` nextInd) tokens
      exprsRange = if null tokens then "" else toText $ Range {from = from $ getRange $ head tokens, to = to $ getRange $ last tokens}
   in [st|#{nL}#{indent ind}#{exprsRange} expressions block#{exprsText}|]

writeSignatureItem :: Int -> Int -> FunctionSignatureItem -> Int -> (Int, Int, Text)
writeSignatureItem nameNum argNum (FunctionName expression) ind =
  ( nameNum - 1,
    argNum,
    let functionNameText = writeExpr expression (ind + 1)
        fnRange = getRange expression
     in [st|#{nL}#{indent ind}#{fnRange} name #{nameNum}#{functionNameText}|]
  )
writeSignatureItem nameNum argNum (FunctionArgument expression) ind =
  ( nameNum,
    argNum - 1,
    let nextInd = ind + 1
        expressionText = case expression of
          (HigherPriorityExpr OcRanged {ocItem = subExprs}) ->
            case subExprs of
              [] -> [st|#{nL}#{indent nextInd}empty argument|]
              expressions ->
                let expressionsText = T.concat $ map (writeExprsBlock (nextInd + 1)) expressions
                 in [st|#{nL}#{indent nextInd}argument in brackets #{argNum}#{expressionsText}|]
          expr -> writeExpr expr nextInd
        argRange = getRange expression
     in [st|#{nL}#{indent ind}#{argRange} argument #{argNum}#{expressionText}|]
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

getFsiRange :: FunctionSignatureItem -> Range
getFsiRange (FunctionName expr) = getRange expr
getFsiRange (FunctionArgument expr) = getRange expr

getExprsBlockRange :: ExprsBlock -> Range
getExprsBlockRange (ExprsBlock exprs) = Range {from = from $ getRange $ head exprs, to = to $ getRange $ last exprs}

writeSignature :: FunctionSignature -> Int -> Text
writeSignature FunctionSignature {fsItems} ind =
  let nextInd = ind + 1
      itemsText = writeFunctionSignatureItems nextInd fsItems
      itemsRange = Range {from = from $ getFsiRange $ head fsItems, to = to $ getFsiRange $ last fsItems}
   in [st|#{nL}#{indent ind}#{itemsRange} function signature#{itemsText}|]

writeExpr :: TokenExpr -> Int -> Text
writeExpr (AlphaNumExpr ranged) = writeRanged "alpha numberic word" writeSingleWord ranged
writeExpr (NonAlphaNumExpr ranged) = writeRanged "non alpha numberic word" writeSingleWord ranged
writeExpr (NumberExpr ranged) =
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
writeExpr (TextExpr ranged) = writeRanged "text" writeSingleWord ranged
writeExpr (NestedExpr expressions) = \ind ->
  let expressionsText = mconcat $ map (writeExprsBlock (ind + 1)) expressions
      exprRange = Range {from = from $ getExprsBlockRange $ head expressions, to = to $ getExprsBlockRange $ last expressions}
   in [st|#{nL}#{indent ind}#{exprRange} nested expressions#{expressionsText}|]
writeExpr (HigherPriorityExpr ranged) =
  writeOcRanged "higher priority expressions" (\nextInd expr -> mconcat $ map (writeExprsBlock nextInd) expr) ranged
writeExpr (SigHigherPriorityExpr ranged) =
  writeOcRanged "higher priority expressions" (\nextInd expr -> mconcat $ map (`writeExpr` nextInd) expr) ranged

writeBody :: Ranged [ExprsBlock] -> Int -> Text
writeBody =
  writeRanged
    "function body"
    (\nextInd expression -> let expressionTexts = map (writeExprsBlock nextInd) expression in mconcat expressionTexts)

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
