{-# LANGUAGE DerivingVia #-}

module AOC1 where

import Control.Comonad ( Comonad(extend) )
import Control.Monad ()
import Data.List.NonEmpty (NonEmpty)
import qualified Data.List.NonEmpty as N
import Witherable ( Filterable(catMaybes) )
import Data.Maybe (fromMaybe)
import Data.Semigroup
import Data.Monoid (getSum)
import Data.Foldable (fold)
import Prelude hiding (sum)

data Change = Increase | Decrease | Unchanged deriving (Eq,Show)

firstPair :: NonEmpty a -> Maybe (a,a)
firstPair (a1 N.:| a2 : _ ) = Just (a1,a2)
firstPair _ = Nothing

change :: Ord a => a -> a -> Change
change a1 a2 | a1 < a2 = Increase
             | a1 == a2 = Unchanged
             | otherwise = Decrease

pairs:: NonEmpty a -> Maybe (NonEmpty (a,a))
pairs =  stripMaybe  . extend firstPair

increase:: Change -> Sum Int
increase Increase = 1
increase Decrease = 0
increase Unchanged = 0

increases :: Ord a => NonEmpty a -> Int
increases = getSum . maybe 0 (foldMap (increase . uncurry change )) . pairs

sum :: Num a => NonEmpty a -> a
sum = getSum . foldMap Sum

takeN :: Int -> NonEmpty a -> Maybe (NonEmpty a)
takeN n (h N.:| t) | n <= 0 = Nothing
                  | n == 1 = Just (N.singleton h)
                  | otherwise = N.nonEmpty t >>= fmap (N.cons h) . takeN (n-1)

sublistN :: Int -> NonEmpty a -> Maybe (NonEmpty (NonEmpty a))
sublistN n = stripMaybe . extend (takeN n)

sumN :: Num a => Int -> NonEmpty a -> Maybe (NonEmpty a)
sumN n =  fmap (fmap sum) . sublistN n

increasesInWindow :: (Num a, Ord a) => Int -> NonEmpty a -> Int
increasesInWindow n =  maybe 0 increases . sumN n

stripMaybe :: NonEmpty (Maybe a) -> Maybe (NonEmpty a)
stripMaybe = N.nonEmpty . catMaybes . N.toList
