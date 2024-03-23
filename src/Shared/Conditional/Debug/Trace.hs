module Shared.Conditional.Debug.Trace
  ( Shared.Conditional.Debug.Trace.trace,
  )
where

import Data.Bool (Bool (False))
import Data.Function (($))
import Data.Text (Text, unpack)
import Debug.Trace as DT (trace)
import System.Environment.MrEnv (envAsBool)
import System.IO.Unsafe (unsafePerformIO)

tracingEnabled :: Bool
{-# NOINLINE tracingEnabled #-}
tracingEnabled = unsafePerformIO $ envAsBool "A_TRACE" False

trace :: Text -> p -> p
trace str expr = if tracingEnabled then DT.trace (unpack str) expr else expr
