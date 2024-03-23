module Main (main) where

import Control.Monad (return)
import Control.Monad.Except (runExceptT)
import Data.Either (Either (Left, Right))
import Data.Function (($))
import Data.List as L (head)
import Data.Text (pack)
import Data.Text.IO as TIO (putStrLn)
import Main.Syntax.Parsing.ParserIO (parseSyntaxIO)
import Shared.Text.Utils (nL)
import System.Environment (getArgs, getProgName)
import System.IO (IO)
import Text.Shakespeare.Text (st)

main :: IO ()
main = do
  TIO.putStrLn "start parsing"
  args <- getArgs
  name <- getProgName
  case args of
    [] -> TIO.putStrLn [st|Usage: #{name} path-to-source-file|]
    _ -> do
      let sourceFile = pack $ L.head args
      resultEx <- runExceptT $ parseSyntaxIO sourceFile
      case resultEx of
        Left err -> TIO.putStrLn [st|Errors: #{nL}#{err}|]
        Right (tree, _) -> do
          TIO.putStrLn [st|--------------------------------------------------------------------------------|]
          TIO.putStrLn [st|Syntax tree: #{tree}|]
          TIO.putStrLn [st|--------------------------------------------------------------------------------|]
      return ()
