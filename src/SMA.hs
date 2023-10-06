{-# LANGUAGE DerivingVia #-}

module SMA where

import Control.Comonad ( Comonad(extend) )
import Control.Monad ()
import Data.List.NonEmpty (NonEmpty)
import qualified Data.List.NonEmpty as N
import Data.Semigroup ( Sum(Sum) )
import Witherable ( Filterable(catMaybes) )

newtype Average a = Average (a, Int)
  deriving (Eq, Show)
  deriving (Semigroup, Monoid) via (Sum a, Sum Int)

runAverage :: (Fractional a) => Average a -> a
runAverage (Average (a, b)) = a / fromIntegral b

calculateAverage :: (Fractional a) => NonEmpty a -> Average a
calculateAverage = foldMap (\a -> Average (a, 1))

average :: (Fractional a) => NonEmpty a -> a
average = runAverage . calculateAverage

takeN :: Int -> NonEmpty a -> Maybe (NonEmpty a)
takeN n (a N.:| as)
  | n <= 0 = Nothing
  | n == 1 = Just (N.singleton a)
  | otherwise = N.nonEmpty as >>= (fmap (N.cons a) . takeN (n - 1))

sublistN :: Int -> NonEmpty a -> Maybe (NonEmpty (NonEmpty a))
sublistN n = stripMaybe . extend (takeN n)
  where
    stripMaybe = N.nonEmpty . catMaybes . N.toList

-- simple moving average
sma :: Fractional a => Int -> NonEmpty a -> Maybe (NonEmpty a)
sma n = fmap (fmap average) . sublistN n
