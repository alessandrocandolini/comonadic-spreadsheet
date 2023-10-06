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
       increases [199, 200, 208, 210, 200, 207,240,269,260,273]  `shouldBe` 7

     before (readNumbers "resources/aoc1.txt") $ do
      it "number of increases reading from a file" $ \testInputs -> do
        (increases testInputs) `shouldBe` 1616
