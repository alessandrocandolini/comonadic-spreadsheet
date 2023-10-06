{-# LANGUAGE OverloadedLists #-}
module SMASpec where

import SMA
import Test.Hspec
import Test.Hspec.QuickCheck
import Test.QuickCheck.Property

spec :: Spec
spec = describe "Simple test" $ do

     it "average of a list" $
        average [1,2,3] `shouldBe` 2

     it "average of a list" $
        average [1.5,4,3.5,1] `shouldBe` 2.5

     it "take N for N < size" $
        takeN 3 [1,2,3,4] `shouldBe` Just [1,2,3]

     it "take N for N > size" $
        takeN 5 [1,2,3,4] `shouldBe` Nothing

     it "take N for N < 0 " $
        takeN (-1) [1,2,3,4] `shouldBe` Nothing

     it "sublistN for N < size" $
        sublistN 3 [1,2,3,4,5,6] `shouldBe` Just [[1,2,3], [2,3,4], [3,4,5], [4,5,6]]

     it "sublistN for N > size" $
        sublistN 5 [1,2,3] `shouldBe` Nothing

     it "sma for N < size" $
        sma 3 [1,2,3,4,5,6] `shouldBe` Just [2,3,4,5]

     it "sma for N > size" $
        sma 5 [1,2,3] `shouldBe` Nothing

     it "example-based unit test" $
        1 `shouldBe` 1

     prop "property-based unit test" $
        \l -> reverse ( reverse l ) == ( l::[Int])
