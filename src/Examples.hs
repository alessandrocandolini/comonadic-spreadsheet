{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE TypeFamilies #-}
module Examples where
import Prelude hiding (iterate, head)
import Control.Comonad
import GHC.Exts

data MyNonEmpty a = MyNonEmpty a [a] deriving (Eq, Show, Functor)

instance IsList (MyNonEmpty a) where
  type Item (MyNonEmpty a) = a
  fromList [] = error "cannot accept empty list"
  fromList (a:as) = MyNonEmpty a as
  toList (MyNonEmpty a as) = a : as

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

sumK :: Num a => WindowSize -> MyNonEmpty a -> Maybe a
sumK (WindowSize k) _ | k <= 0 = Nothing
sumK (WindowSize 1) (MyNonEmpty a _) = Just a
sumK k (MyNonEmpty a as) = case nonEmpty as of
   Just as2 -> (+a) <$> sumK (k-1) as2
   Nothing -> Nothing


smaLocal :: WindowSize -> MyNonEmpty Double -> Maybe Double
smaLocal (WindowSize k) s = (/ (fromIntegral k)) <$> sumK (WindowSize k) s


sma :: WindowSize -> MyNonEmpty Double -> MyNonEmpty (Maybe Double)
sma size = coFlatMap (smaLocal size)


data Stream a = Stream a (Stream a) deriving (Eq, Show,Functor)

iterate :: (a -> a) -> a -> Stream a
iterate f seed = Stream seed (iterate f (f seed))

instance Comonad Stream where
   extract (Stream a _) = a
   duplicate s'@(Stream _ as) = Stream s' (duplicate as)

first :: Stream a -> a
first (Stream a _) = a

intStream :: Int -> Stream Int
intStream n = Stream n (intStream (n + 1))

allNumbers :: Stream Int
allNumbers = intStream 0

takeN :: Int -> Stream a -> [a]
takeN 0 _ = []
takeN n (Stream a as) = a : takeN (n-1) as

data Sheet1 a = Sheet1 (Stream a) a (Stream a) deriving (Eq,Show,Functor)

focus :: Sheet1 a -> a
focus (Sheet1 _ a _ ) = a

moveL :: Sheet1 a -> Sheet1 a
moveL (Sheet1 (Stream a as) focus s) = Sheet1 as a (Stream focus s)

moveR :: Sheet1 a -> Sheet1 a
moveR (Sheet1 s focus (Stream a as)) = Sheet1 (Stream focus s) a as

allLeft :: Sheet1 a -> Stream (Sheet1 a)
allLeft = iterate moveL

allRight:: Sheet1 a -> Stream (Sheet1 a)
allRight = iterate moveR


instance Comonad Sheet1 where
  extract = focus
  duplicate s = Sheet1 (allLeft s) s (allRight s)



