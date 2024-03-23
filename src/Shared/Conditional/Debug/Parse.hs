module Shared.Conditional.Debug.Parse
  ( Shared.Conditional.Debug.Parse.dbg,
  )
where

import Data.Bool (Bool (False))
import Data.Function (($))
import Data.Text (Text, unpack)
import System.Environment.MrEnv (envAsBool)
import System.IO.Unsafe (unsafePerformIO)
import Text.Megaparsec.Debug (MonadParsecDbg)
import Text.Megaparsec.Debug as D (dbg)
import Text.Show (Show)

tracingEnabled :: Bool
{-# NOINLINE tracingEnabled #-}
tracingEnabled = unsafePerformIO $ envAsBool "A_DEBUG" False

dbg :: forall e s m a. (MonadParsecDbg e s m, Show a) => Text -> m a -> m a
dbg str m = if tracingEnabled then D.dbg (unpack str) m else m
