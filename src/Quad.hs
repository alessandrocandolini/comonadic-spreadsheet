{-# LANGUAGE DerivingVia #-}

module Quad where

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

