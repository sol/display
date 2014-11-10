module Data.Display (
  Display (..)
, display
) where

import           Data.Text (Text)
import           Data.Text.Lazy (toStrict)
import           Data.Display.Lazy (Display(..))
import qualified Data.Display.Lazy as Lazy

display :: Display a => a -> Text
display = toStrict . Lazy.display
