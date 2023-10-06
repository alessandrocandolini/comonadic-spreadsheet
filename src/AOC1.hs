{-# LANGUAGE DerivingVia #-}

module AOC1 where

import Control.Comonad ( Comonad(extend) )
import Control.Monad ()
import Data.List.NonEmpty (NonEmpty)
import qualified Data.List.NonEmpty as N
import Witherable ( Filterable(catMaybes) )
import Data.Maybe (fromMaybe)
import Data.Semigroup (Sum)
import Data.Monoid (getSum)

data Change = Increase | Decrease deriving (Eq,Show)

firstPair :: NonEmpty a -> Maybe (a,a)
firstPair (a1 N.:| a2 : _ ) = Just (a1,a2)
firstPair _ = Nothing

change :: Ord a => a -> a -> Change
change a1 a2 | a1 <= a2 = Increase
             | otherwise = Decrease

pairs:: NonEmpty a -> Maybe (NonEmpty (a,a))
pairs =  stripMaybe  . extend firstPair

increase:: Change -> Sum Int
increase Increase = 1
increase Decrease = 0

increases :: NonEmpty Int -> Int
increases = getSum . maybe 0 (foldMap (increase . uncurry change )) . pairs

stripMaybe :: NonEmpty (Maybe a) -> Maybe (NonEmpty a)
stripMaybe = N.nonEmpty . catMaybes . N.toList
