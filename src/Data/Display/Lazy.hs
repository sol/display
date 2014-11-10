{-# LANGUAGE TypeSynonymInstances, FlexibleInstances, DefaultSignatures #-}
module Data.Display.Lazy where

import           Data.Text.Lazy (Text)
import           Data.Text.Lazy.Builder
import           Data.Text.Lazy.Builder.Int (decimal)
import           Data.Text.Lazy.Builder.RealFloat
import qualified Data.Text as S

class Display a where
  displayBuilder :: a -> Builder
  default displayBuilder :: Show a => a -> Builder
  displayBuilder = fromString . show

display :: Display a => a -> Text
display = toLazyText . displayBuilder

instance Display String where
  displayBuilder = fromString

instance Display S.Text where
  displayBuilder = fromText

instance Display Text where
  displayBuilder = fromLazyText

instance Display Int where
  displayBuilder = decimal

instance Display Integer where
  displayBuilder = decimal

instance Display Float where
  displayBuilder = realFloat

instance Display Double where
  displayBuilder = realFloat
