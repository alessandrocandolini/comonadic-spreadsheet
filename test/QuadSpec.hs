{-# LANGUAGE OverloadedLists #-}
module QuadSpec where

import Quad
import Test.Hspec
import Test.Hspec.QuickCheck
import Test.QuickCheck.Property
import Data.List.NonEmpty (fromList, NonEmpty)
import qualified Data.Text as T
import qualified Data.Text.IO as T
import Data.Foldable (forM_)

spec :: Spec
spec = describe "Quad tests" $ do

     it "simple test" $
       5 `shouldBe` 5
