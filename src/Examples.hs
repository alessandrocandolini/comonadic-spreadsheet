{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE TypeFamilies #-}
module Examples where
import Prelude hiding (head)
import Control.Comonad
import GHC.Exts

data MyNonEmpty a = MyNonEmpty a [a] deriving (Eq, Show, Functor)

instance IsList (MyNonEmpty a) where
  type Item (MyNonEmpty a) = a
  fromList = from
  toList = to

from [] = error "cannot accept empty list"
from (a:as) = MyNonEmpty a as

to (MyNonEmpty a as) = a : as


nonEmpty :: [a] -> Maybe (MyNonEmpty a )
nonEmpty [] = Nothing
nonEmpty (h:t) = Just (MyNonEmpty h t)

instance Semigroup (MyNonEmpty a) where
  (MyNonEmpty a1 t1) <> (MyNonEmpty a2 t2) = MyNonEmpty a1 (t1 ++ (a2 : t2))


head :: MyNonEmpty a -> a
head (MyNonEmpty a _ ) = a

tails :: MyNonEmpty a -> MyNonEmpty (MyNonEmpty a)
tails n@(MyNonEmpty _ t) =  case nonEmpty t of
   Nothing -> MyNonEmpty n []
   Just t1 -> MyNonEmpty n [] <> tails t1

instance Comonad MyNonEmpty where
  extract = head
  duplicate = tails


coFlatMap :: Comonad w => (w a -> b) -> w a -> w b
coFlatMap f = fmap f . duplicate

newtype WindowSize = WindowSize Int deriving (Eq,Show)
    deriving (Num,Ord,Read) via Int

sma :: Num a => WindowSize -> MyNonEmpty a -> [Double]
sma size = const []

