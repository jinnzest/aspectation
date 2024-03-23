module Pure.Main.Syntax.Shared
  ( runParser,
    withBorder,
  )
where

import Control.Monad.Except (runExceptT)
import Data.Either (Either)
import Data.Function (($))
import Data.Functor.Identity (Identity (runIdentity))
import Data.String (String)
import Data.Text (Text, unpack)
import Shared.Errors (Errors)
import Shared.Parser.Data (Parser)
import Shared.Parser.Wrapper (parse)
import Shared.Text.Utils (nL)
import Text.Shakespeare.Text (st)

runParser :: Text -> Parser (a, Text) -> Either Errors (a, Text)
runParser body parser = runIdentity $ runExceptT $ parse "" body parser

withBorder :: Text -> String
withBorder text = unpack [st|--------------------------------------------------------------------------------#{nL}#{text}#{nL}|]
