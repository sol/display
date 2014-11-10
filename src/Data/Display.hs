{-# LANGUAGE TypeSynonymInstances, FlexibleInstances, DefaultSignatures #-}
module Data.Display where

import           Data.Text (Text)
import           Data.Text.Internal.Builder
import           Data.Text.Lazy (toStrict)
import qualified Data.Text.Lazy as LT

class Display a where
  displayBuilder :: a -> Builder
  default displayBuilder :: Show a => a -> Builder
  displayBuilder = fromString . show

display :: Display a => a -> Text
display = toStrict . toLazyText . displayBuilder

instance Display String where
  displayBuilder = fromString

instance Display Text where
  displayBuilder = fromText

instance Display LT.Text where
  displayBuilder = fromLazyText

instance Display Int
instance Display Integer
instance Display Float
instance Display Double
