module Sheet where

import Data.Function (fix)

fix1 :: (a -> a) -> a
fix1 f = a where a = f a

class CoFlatMap f where
  coFlatMap :: (f a -> b) -> f a -> f b

loeb :: (Functor f) => f (f a -> a) -> f a
loeb ff = fix1 $ \fa -> fmap ($ fa) ff

class (Functor f) => Comonad f where
  extract :: f a -> a
  cojoin :: f a -> f (f a)


data Stream a = Stream a (Stream a) deriving (Eq, Show)
