{-# LANGUAGE OverloadedStrings #-}
module Data.DisplaySpec (main, spec) where

import           Test.Hspec
import           Test.QuickCheck
import           Test.QuickCheck.Instances ()

import           Data.String
import qualified Data.Text.Lazy as LT
import           Data.Display

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "display" $ do
    context "when used with string" $ do
      it "behaves like `fromString`" $ do
        property $ \s -> display (s :: String) `shouldBe` fromString s

    context "when used with text" $ do
      it "behaves like `id`" $ do
        property $ \s -> display s `shouldBe` s

    context "when used with lazy text" $ do
      it "behaves like `toStrict`" $ do
        property $ \s -> display (s :: LT.Text) `shouldBe` LT.toStrict s

    context "when used with Int" $ do
      it "behaves like `fromString . show`" $ do
        display (23 :: Int) `shouldBe` "23"
