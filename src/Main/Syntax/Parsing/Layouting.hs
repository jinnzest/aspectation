module Main.Syntax.Parsing.Layouting (mkLayouts) where

import Data.Bool (Bool (False, True), otherwise)
import Data.Eq ((==))
import Data.Function (($), (.))
import Data.Int (Int)
import Data.Kind (Type)
import Data.List (filter, init)
import Data.List as L (foldr, last, reverse, (++))
import Data.Ord (Ord ((<), (<=), (>)))
import Data.Tuple (snd)
import Main.Syntax.Parsing.Shared (getRange)
import Main.Syntax.Parsing.Tree
  ( ExprsBlock (ExprsBlock),
    TokenExpr (NestedExpr),
  )
import Shared.Location.Data (Position (column, line), Range (from, to))

type LayoutingCtx :: Type
data LayoutingCtx = LayoutingCtx
  { baseBlockIndent :: Int,
    baseBlockLine :: Int,
    processed :: [ExprsBlock],
    unprocessed :: [ExprsBlock]
  }

lineExprStarts :: TokenExpr -> Int
lineExprStarts = line . from . getRange

lineExprEnds :: TokenExpr -> Int
lineExprEnds = line . to . getRange

foldByLines :: TokenExpr -> (Int, [ExprsBlock]) -> (Int, [ExprsBlock])
foldByLines _ (_, []) = (0, [])
foldByLines expr (lastLineFrom, block@(ExprsBlock exprs : tail)) =
  let currLineFrom = lineExprStarts expr
      currLineTo = lineExprEnds expr
      newBlock =
        if currLineTo == lastLineFrom
          then ExprsBlock (expr : exprs) : tail
          else ExprsBlock [expr] : block
   in (currLineFrom, newBlock)

splitByLines :: [TokenExpr] -> [ExprsBlock]
splitByLines exprs =
  let lastExpr = last exprs
      initExprs = init exprs
      lastExprBlock = [ExprsBlock [lastExpr]]
      lineLastExprStarts = lineExprStarts lastExpr
      result = foldr foldByLines (lineLastExprStarts, lastExprBlock) initExprs
   in snd result

applyHeadOrZero :: (t -> Int) -> [t] -> Int
applyHeadOrZero f lst = case lst of
  [] -> 0
  h : _ -> f h

layouting :: LayoutingCtx -> LayoutingCtx
layouting ctx@LayoutingCtx {unprocessed = []} = ctx
layouting ctx@LayoutingCtx {processed = [], unprocessed = unprocessedHead : unprocessedTail} =
  layouting ctx {processed = [unprocessedHead], unprocessed = unprocessedTail}
layouting
  ctx@LayoutingCtx
    { baseBlockIndent,
      baseBlockLine,
      processed = processedExprs@(processedExprsHead@(ExprsBlock processedBlock) : processedTail),
      unprocessed = unprocessedExprs@(unprocessedHead@(ExprsBlock unprocessedBlock) : unprocessedTail)
    } =
    let itemIndent = column . from . getRange
        processedExprsLine = lineExprEnds $ last processedBlock
        unprocessedIndent = applyHeadOrZero itemIndent unprocessedBlock
        filterOutNestedExpr = filter (\case NestedExpr _ -> False; _ -> True)
        exprsBlockHeadIndent (ExprsBlock exprs) = applyHeadOrZero itemIndent $ filterOutNestedExpr exprs
        processedIndent
          | baseBlockLine == processedExprsLine = 0
          | otherwise = exprsBlockHeadIndent processedExprsHead
        layoutSmallerIndention =
          layouting
            ctx
              { processed = [ExprsBlock (NestedExpr (reverse processedExprs) : unprocessedBlock)],
                unprocessed = unprocessedTail
              }
        layoutEqualIndention =
          layouting
            ctx
              { processed = unprocessedHead : processedExprs,
                unprocessed = unprocessedTail
              }
        layoutBiggerIndention =
          let nested =
                layouting
                  ctx
                    { baseBlockIndent = processedIndent,
                      processed = [],
                      unprocessed = unprocessedExprs
                    }
              processedNested = reverse $ processed nested
              unprocessedNested = unprocessed nested
           in layouting
                ctx
                  { baseBlockIndent,
                    processed = ExprsBlock (processedBlock ++ [NestedExpr processedNested]) : processedTail,
                    unprocessed = unprocessedNested
                  }
     in if unprocessedIndent <= baseBlockIndent
          then ctx
          else
            if unprocessedIndent > processedIndent
              then layoutBiggerIndention
              else
                if unprocessedIndent < processedIndent
                  then layoutSmallerIndention
                  else layoutEqualIndention

mkLayouts :: Int -> [TokenExpr] -> [ExprsBlock]
mkLayouts baseBlockLine expressions =
  reverse $
    processed $
      layouting
        LayoutingCtx
          { baseBlockIndent = 0,
            baseBlockLine,
            processed = [],
            unprocessed = splitByLines expressions
          }
