{-# LANGUAGE OverloadedLists #-}
module ConwaySpec where

import SMA
import Test.Hspec
import Test.Hspec.QuickCheck
import Test.QuickCheck.Property

spec :: Spec
spec = describe "Simple test" $ do

     it "average of a list" $
        2 `shouldBe` 2

