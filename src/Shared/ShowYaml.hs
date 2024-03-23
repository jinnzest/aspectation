module Shared.ShowYaml
  ( showYaml,
  )
where

import Data.Aeson (ToJSON)
import Data.Aeson.Yaml (encode)
import Data.ByteString.Lazy.UTF8 (toString)
import Data.Function ((.))
import Data.String (String)

showYaml :: ToJSON a => a -> String
showYaml = toString . encode
