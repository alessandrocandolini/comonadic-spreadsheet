{-# LANGUAGE OverloadedLists #-}
module ExamplesSpec where

import Examples
import Test.Hspec
import Test.Hspec.QuickCheck
import Test.QuickCheck.Property

spec :: Spec
spec = describe "Simple test" $ do

     it "example-based unit test" $
        1 `shouldBe` 1

     it "tails of a nonempty list of multiple elements" $
        tails (MyNonEmpty 1 [2,3]) `shouldBe` (MyNonEmpty (MyNonEmpty 1 [2,3]) [(MyNonEmpty 2 [3]), (MyNonEmpty 3 [])])

     it "tails of a nonempty list of one element" $
        tails (MyNonEmpty 1 []) `shouldBe` (MyNonEmpty (MyNonEmpty 1 []) [])

     it "tails of a nonempty list of multiple elements (overloaded syntax)" $
        tails [1,2,3] `shouldBe` [[1,2,3], [2, 3], [3]]

     it "tails of a nonempty list of one element (overloaded syntax)" $
        tails [1] `shouldBe` [[1]]

     prop "property-based unit test" $
        \l -> reverse ( reverse l ) == ( l::[Int])
