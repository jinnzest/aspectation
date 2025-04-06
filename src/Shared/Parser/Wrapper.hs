module Shared.Parser.Wrapper
  ( Shared.Parser.Wrapper.parse,
  )
where

import Control.Monad (Monad (return))
import Control.Monad.Except (ExceptT, throwError)
import Data.Either (Either (Left, Right))
import Data.Function (($))
import Data.Functor.Identity (Identity)
import Data.Text (Text, pack, unpack)
import Shared.Errors (Error (Error), Errors (Errors))
import Shared.Parser.Data (Parser)
import Text.Megaparsec as MP (parse)
import Text.Megaparsec.Error (errorBundlePretty)
import Text.Shakespeare.Text ()

parse :: Text -> Text -> Parser (p, Text) -> ExceptT Errors Identity (p, Text)
parse filePath fileBody parser = case MP.parse parser (unpack filePath) fileBody of
  Left bundle -> throwError $ Errors [Error $ pack $ errorBundlePretty bundle]
  Right res -> return res
