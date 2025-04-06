module Main.Syntax.Parsing.Shared (getRange) where

import Data.Function (($))
import Data.List (head, last)
import Main.Syntax.Parsing.Tree (ExprsBlock (ExprsBlock), TokenExpr (AlphaNumExpr, HigherPriorityExpr, NestedExpr, NonAlphaNumExpr, NumberExpr, SigHigherPriorityExpr, TextExpr))
import Shared.Location.Data (OcRanged (ocRange), Range (Range, from, to), Ranged (range))

getRange :: TokenExpr -> Range
getRange (AlphaNumExpr i) = range i
getRange (NonAlphaNumExpr i) = range i
getRange (NumberExpr i) = range i
getRange (TextExpr i) = range i
getRange (NestedExpr expressions) =
  Range
    { from = let ExprsBlock exprs = head expressions in from $ getRange $ head exprs,
      to = let ExprsBlock exprs = last expressions in to $ getRange $ last exprs
    }
getRange (HigherPriorityExpr e) = ocRange e
getRange (SigHigherPriorityExpr e) = ocRange e
