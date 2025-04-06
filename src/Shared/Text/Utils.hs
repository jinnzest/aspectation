module Shared.Text.Utils
  ( nL,
    tab,
    indent,
    hasNL,
    withBorder,
  )
where

import Data.Bool (Bool, (||))
import Data.Int (Int)
import Data.Monoid (Monoid (mconcat))
import Data.Text (Text, isInfixOf, singleton)
import Text.Shakespeare.Text (st)

nL :: Text
nL = singleton '\n'

tab :: Text
tab = singleton '\t'

indent :: Int -> Text
indent n = mconcat [singleton '\t' | _ <- [1 .. n]]

hasNL :: Text -> Bool
hasNL t = isInfixOf "\n" t || isInfixOf "\r" t

withBorder :: Text -> Text
withBorder text = [st|--------------------------------------------------------------------------------#{nL}#{text}#{nL}|]
