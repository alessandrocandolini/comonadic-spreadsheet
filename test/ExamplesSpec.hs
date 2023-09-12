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

     it "smaLocal simple list" $
        smaLocal (WindowSize 4) [1,2,3,4,5,6,7,8,9,10] `shouldBe` Just 2.5

     it "smaLocal insufficient list" $
        smaLocal (WindowSize 4) [1,2,3] `shouldBe` Nothing


     it "sma simple list size 1" $
        sma (WindowSize 1) [1,2,3,4,5,6,7,8,9,10] `shouldBe` (Just <$> [1,2,3,4,5,6,7,8,9,10])

     it "sma simple list" $
        sma (WindowSize 3) [1,2,3,4,5,6,7,8,9,10] `shouldBe` (Just <$> [2,3,4,5,6,7,8,9] ) <> [Nothing, Nothing]

     it "allNumbers" $
        takeN 10 allNumbers `shouldBe` [0,1,2,3,4,5,6,7,8,9]

     prop "property-based unit test" $
        \l -> reverse ( reverse l ) == ( l::[Int])
