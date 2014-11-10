{-# LANGUAGE TypeSynonymInstances, FlexibleInstances, DefaultSignatures #-}
module Data.Display where

import           Data.Text (Text)
import           Data.Text.Lazy.Builder
import           Data.Text.Lazy (toStrict)
import qualified Data.Text.Lazy as LT
import           Data.Text.Lazy.Builder.Int (decimal)
import           Data.Text.Lazy.Builder.RealFloat

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

instance Display Int where
  displayBuilder = decimal

instance Display Integer where
  displayBuilder = decimal

instance Display Float where
  displayBuilder = realFloat

instance Display Double where
  displayBuilder = realFloat
