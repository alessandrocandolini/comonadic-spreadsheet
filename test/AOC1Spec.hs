{-# LANGUAGE OverloadedLists #-}
module AOC1Spec where

import AOC1
import Test.Hspec
import Test.Hspec.QuickCheck
import Test.QuickCheck.Property
import Data.List.NonEmpty (fromList, NonEmpty)
import qualified Data.Text as T
import qualified Data.Text.IO as T
import Data.Foldable (forM_)

readFileContent :: Read a => FilePath -> IO [a]
readFileContent =  fmap (fmap (read . T.unpack) . T.lines) . T.readFile

readNumbers :: FilePath -> IO (NonEmpty Int)
readNumbers = fmap fromList . readFileContent

spec :: Spec
spec = describe "AOC2021 day1 https://adventofcode.com/2021/day/1" $ do

     it "number of increases on a small example list" $
       increases [199, 200, 208, 210, 200, 207,240,269,260,263]  `shouldBe` 7

     it "sums in window on a small example list" $
       sumN 3 [199, 200, 208, 210, 200, 207,240,269,260,263]  `shouldBe` Just [607, 618, 618, 617, 647, 716, 769, 792]

     it "number of increases in window on a small example list" $
       increasesInWindow 3 [199, 200, 208, 210, 200, 207,240,269,260,263]  `shouldBe` 5

     before (readNumbers "resources/aoc1.txt") $ do
       it "number of increases reading from a file" $ \testInputs -> do
         (increases testInputs) `shouldBe` 1616

       it "number of increases reading from a file" $ \testInputs -> do
         (increasesInWindow  3 testInputs) `shouldBe` 1645
