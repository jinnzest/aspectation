module Shared.Text.Utils
  ( nL,
    indent,
    withBorder,
  )
where

import Data.Int (Int)
import Data.Monoid (Monoid (mconcat))
import Data.Text (Text, singleton)
import Text.Shakespeare.Text (st)

nL :: Text
nL = singleton '\n'

indent :: Int -> Text
indent n = mconcat [singleton '\t' | _ <- [1 .. n]]

withBorder :: Text -> Text
withBorder text = [st|--------------------------------------------------------------------------------#{nL}#{text}#{nL}|]
