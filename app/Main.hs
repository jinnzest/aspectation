module Main (main) where

import Control.Monad (return)
import Data.Text.IO as DTIO (putStrLn)
import System.IO (IO)

main :: IO ()
main = do
  DTIO.putStrLn "starting"
  DTIO.putStrLn "here the system will run"
  DTIO.putStrLn "ending"
  return ()
