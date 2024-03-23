module Shared.Parser.WrapperIO
  ( parseFile,
  )
where

import Control.Monad (return)
import Control.Monad.Except (ExceptT, liftIO, mapExceptT)
import Data.Function (($), (.))
import Data.Functor.Identity (runIdentity)
import Data.Text (Text, unpack)
import Data.Text.IO as DTIO (putStrLn, readFile)
import Shared.Conditional.Debug.Trace (trace)
import Shared.Errors (Errors)
import Shared.Parser.Data (Parser)
import Shared.Parser.Wrapper (parse)
import System.Directory (doesFileExist)
import System.IO (IO)
import Text.Shakespeare.Text (st)

parseFile :: Text -> Text -> Parser ([p], Text) -> ExceptT Errors IO ([p], Text)
parseFile message filePath parser = do
  let filePathT = unpack filePath
  liftIO $ DTIO.putStrLn [st|#{message}|]
  fileExists <- liftIO $ doesFileExist filePathT
  if fileExists
    then do
      fileBody <- liftIO $ readFile filePathT
      trace [st|source: #{fileBody}|] $ mapExceptT (return . runIdentity) $ parse filePath fileBody parser
    else do
      liftIO $ DTIO.putStrLn [st|File '#{filePath}' was not found|]
      return ([], "")
