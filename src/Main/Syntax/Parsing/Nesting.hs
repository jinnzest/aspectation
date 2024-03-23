module Main.Syntax.Parsing.Nesting (mkNestedExprs) where

import Data.Bool (otherwise)
import Data.Eq (Eq, (==))
import Data.Function (($), (.))
import Data.Int (Int)
import Data.Kind (Type)
import Data.List (foldl, foldr, head, last)
import Data.List.NonEmpty (NonEmpty, fromList, map, reverse, singleton, toList)
import Data.List.NonEmpty.Extra (NonEmpty ((:|)))
import Data.Ord (Ord ((>)))
import Data.String (fromString)
import Deriving.Aeson
  ( CustomJSON (CustomJSON),
    Generic,
    ToJSON,
  )
import Deriving.Aeson.Stock (Vanilla)
import Main.Syntax.Parsing.Tree
  ( Expression
      ( AlphaNumExpr,
        HigherPriorityExpr,
        NestedExpr,
        NonAlphaNumExpr,
        NumberExpr,
        TextExpr
      ),
  )
import Shared.Location.Data
  ( OcRanged (OcRanged, ocItem, ocRange),
    Position (Position, column, line),
    Range (Range, from, to),
    Ranged (range),
  )
import Shared.ShowYaml (showYaml)
import Text.Shakespeare.Text (ToText (toText))

type Nesting :: Type
data Nesting
  = NestedExpressions
      { indent :: Int,
        itemLine :: Int,
        items :: NonEmpty Nesting
      }
  | SingleExpression Expression
  deriving stock (Eq, Generic)
  deriving (ToJSON) via Vanilla Nesting

instance ToText Nesting where
  toText = fromString . showYaml

instance ToText [Nesting] where
  toText = fromString . showYaml

getRange :: Expression -> Range
getRange (AlphaNumExpr i) = range i
getRange (NonAlphaNumExpr i) = range i
getRange (NumberExpr i) = range i
getRange (TextExpr i) = range i
getRange (NestedExpr expressions) =
  Range
    { from = from $ getRange $ head expressions,
      to = to $ getRange $ last expressions
    }
getRange (HigherPriorityExpr e) = ocRange e

itemIndent :: Expression -> Int
itemIndent = column . from . getRange

getLine :: Expression -> Int
getLine = line . from . getRange

mapHigherPriorityExpressions :: Expression -> Expression
mapHigherPriorityExpressions
  ( HigherPriorityExpr
      ocRanged@OcRanged
        { ocRange = Range {from = Position {line}},
          ocItem
        }
    ) =
    HigherPriorityExpr
      ocRanged
        { ocItem = mkNestedExprs line ocItem
        }
mapHigherPriorityExpressions otherExpression = otherExpression

mapItemLine :: Int -> NonEmpty Nesting -> Int
mapItemLine
  _
  ( ( SingleExpression
        ( HigherPriorityExpr
            OcRanged
              { ocRange = Range {to = Position {line}}
              }
          )
      )
      :| _
    ) = line
mapItemLine itemLine _ = itemLine

theSameLineNesting :: Expression -> Int -> Int -> NonEmpty Nesting -> Nesting
theSameLineNesting expr itemLine indent items@((SingleExpression _) :| _) =
  NestedExpressions
    { indent,
      itemLine,
      items = SingleExpression expr :| toList items
    }
theSameLineNesting expr itemLine indent (itemsHead :| itemsTail) =
  NestedExpressions
    { indent,
      itemLine,
      items = expressionsToNesting itemsHead expr :| itemsTail
    }

theSameIndentNesting :: Expression -> Int -> Int -> NonEmpty Nesting -> Nesting
theSameIndentNesting expr itemLine indent items =
  NestedExpressions
    { indent,
      itemLine,
      items = SingleExpression expr :| toList items
    }

biggerIndentNesting :: Int -> Expression -> Int -> Int -> NonEmpty Nesting -> Nesting
biggerIndentNesting exprIndent expr itemLine indent items@((SingleExpression _) :| _) =
  NestedExpressions
    { indent,
      itemLine,
      items =
        NestedExpressions
          { indent = exprIndent,
            itemLine,
            items = singleton (SingleExpression expr)
          }
          :| toList items
    }
biggerIndentNesting _ expr itemLine indent (itemsHead :| itemsTail) =
  NestedExpressions
    { indent,
      itemLine,
      items = expressionsToNesting itemsHead expr :| itemsTail
    }

smallerIndentNesting :: Int -> Expression -> Int -> Int -> NonEmpty Nesting -> Nesting
smallerIndentNesting exprIndent expr itemLine indent items =
  NestedExpressions
    { indent = exprIndent,
      itemLine,
      items = SingleExpression expr :| [NestedExpressions {indent, itemLine, items}]
    }

expressionsToNesting :: Nesting -> Expression -> Nesting
expressionsToNesting NestedExpressions {indent, itemLine, items} expr =
  let expression = mapHigherPriorityExpressions expr
      exprLine = getLine expression
      exprIndent = itemIndent expression
      mappedItemLine = mapItemLine itemLine items
   in if
          | exprLine == mappedItemLine ->
              theSameLineNesting expression exprLine indent items
          | exprIndent == indent ->
              theSameIndentNesting expression exprLine indent items
          | exprIndent > indent ->
              biggerIndentNesting exprIndent expression exprLine indent items
          | otherwise ->
              smallerIndentNesting exprIndent expression exprLine indent items
expressionsToNesting i _ = i

linesNesting :: NonEmpty Expression -> [Expression]
linesNesting (headExpr :| tailExpr) =
  let initial =
        NestedExpressions
          { indent = itemIndent headExpr,
            itemLine = line $ from $ getRange headExpr,
            items = singleton $ SingleExpression headExpr
          }
      arrowLineNesting = foldl expressionsToNesting initial tailExpr
   in case nestingToExpressions arrowLineNesting of
        NestedExpr expressions -> expressions
        expressions -> [expressions]

nestingToExpressions :: Nesting -> Expression
nestingToExpressions (SingleExpression expression) = expression
nestingToExpressions NestedExpressions {items} =
  case toList $ reverse $ map nestingToExpressions items of
    [nested@(NestedExpr _)] -> nested
    nested -> NestedExpr nested

arrowAndNextLinesNesting :: Int -> NonEmpty Expression -> NonEmpty Expression -> [Expression]
arrowAndNextLinesNesting arrowLine (exprHead :| exprTail) nextLinesExprs =
  let initial = NestedExpressions {indent = 1, itemLine = arrowLine, items = singleton $ SingleExpression exprHead}
      arrowLineNesting = foldl expressionsToNesting initial exprTail
      nextLinesNesting = foldl expressionsToNesting arrowLineNesting nextLinesExprs
   in case nestingToExpressions nextLinesNesting of
        NestedExpr expressions -> expressions
        expressions -> [expressions]

splitArrowAndNextLines :: Int -> [Expression] -> ([Expression], [Expression])
splitArrowAndNextLines arrowLine =
  foldr
    (\e (arrowLines, nextLines) -> if line (from $ getRange e) == arrowLine then (e : arrowLines, nextLines) else (arrowLines, e : nextLines))
    ([], [])

mkNestedExprs :: Int -> [Expression] -> [Expression]
mkNestedExprs arrowLine expressions =
  case splitArrowAndNextLines arrowLine expressions of
    ([], []) -> []
    (arrowLineExprs, []) -> linesNesting $ fromList arrowLineExprs
    ([], nextLinesExprs) -> linesNesting $ fromList nextLinesExprs
    (arrowLineExprs, nextLinesExprs) -> arrowAndNextLinesNesting arrowLine (fromList arrowLineExprs) (fromList nextLinesExprs)
