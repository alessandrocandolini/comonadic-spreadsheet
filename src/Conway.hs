{-# LANGUAGE DerivingVia #-}

module Conway where

import Control.Comonad ( Comonad(extend) )
import Control.Monad ()
import Data.List.NonEmpty (NonEmpty)
import qualified Data.List.NonEmpty as N
import Data.Semigroup ( Sum(Sum) )
import Witherable ( Filterable(catMaybes) )



