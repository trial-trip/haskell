{-# LANGUAGE GeneralisedNewtypeDeriving #-}
{-# LANGUAGE RankNTypes                 #-}
{-# LANGUAGE TypeOperators              #-}

module Stepik2_2_3 where

import           Control.Applicative (liftA2, liftA3)
import           Data.Foldable
import           Data.List           (sort, transpose)

import           Data.List.Split     (chunksOf)
import           Data.Traversable    (fmapDefault, foldMapDefault)

data OddC a
  = Un a
  | Bi a a (OddC a)
  deriving (Eq, Show)

instance Functor OddC where
  fmap f (Un a)     = Un $ f a
  fmap f (Bi a b t) = Bi (f a) (f b) (fmap f t)

instance Foldable OddC where
  foldr f ini (Un a)     = f a ini
  foldr f ini (Bi a b t) = f a $ f b $ foldr f ini t
  foldl f ini (Un a)     = f ini a
  foldl f ini (Bi a b t) = f (f (foldl f ini t) b) a

instance Traversable OddC where
  traverse f (Un a)     = Un <$> f a
  traverse f (Bi a b t) = liftA3 Bi (f a) (f b) (traverse f t)

joinWithRemainder :: OddC a -> OddC a -> (a, OddC a)
joinWithRemainder (Un b) x = (b, x)
joinWithRemainder (Bi a b rest) x = (a, Bi b c t)
  where
    (c, t) = joinWithRemainder rest x

anotherJoin :: OddC a -> (a, OddC a) -> OddC a
anotherJoin (Un a) (b, rest)       = Bi a b rest
anotherJoin (Bi a b more) restPair = Bi a b $ anotherJoin more restPair

concat3OC :: OddC a -> OddC a -> OddC a -> OddC a
concat3OC xs ys zs = anotherJoin xs $ joinWithRemainder ys zs

concatOC :: OddC (OddC a) -> OddC a
concatOC (Un zs)         = zs
concatOC (Bi xs ys rest) = concat3OC xs ys $ concatOC rest

instance Applicative OddC where
  pure = Un
  a1 <*> a2 = a1 >>= \f -> f <$> a2

instance Monad OddC where
  m >>= f = concatOC $ f <$> m

cnt1 = Un 42

cnt3 = Bi 1 2 cnt1

cnt5 = Bi 3 4 cnt3

cntFunctor = (+ 1) <$> cnt5 -- Bi 4 5 (Bi 2 3 (Un 43))

cntFoldable1 = toList cnt5 -- [3,4,1,2,42]

cntFoldable2 = sum cnt5 -- 52

cntTraversable = traverse (\x -> [x + 2, x - 2]) cnt1 -- [Un 44,Un 40]

concatTest1 = Bi 'a' 'b' (Un 'c')

concatTest2 = Bi 'd' 'e' (Bi 'f' 'g' (Un 'h'))

concatTest3 = Bi 'i' 'j' (Un 'k')

concatOddsExample = concat3OC concatTest1 concatTest2 concatTest3
  -- Bi 'a' 'b' (Bi 'c' 'd' (Bi 'e' 'f' (Bi 'g' 'h' (Bi 'i' 'j' (Un 'k')))))

tst1 = Bi 10 20 (Un 30)

tst2 = Bi 1 2 (Bi 3 4 (Un 5))

oddMonad1 = do
  x <- tst1
  y <- tst2
  return (x + y)
  --Bi 11 12 (Bi 13 14 (Bi 15 21 (Bi 22 23 (Bi 24 25 (Bi 31 32 (Bi 33 34 (Un 35)))))))

oddMonad2 = do
  x <- tst2
  y <- tst1
  return (x + y)
  --Bi 11 21 (Bi 31 12 (Bi 22 32 (Bi 13 23 (Bi 33 14 (Bi 24 34 (Bi 15 25 (Un 35)))))))

newtype Temperature a =
  Temperature Double
  deriving (Num, Show, Eq, Fractional)

data Celsius

data Fahrenheit

data Kelvin

comfortTemperature :: Temperature Celsius
comfortTemperature = Temperature 23

c2f :: Temperature Celsius -> Temperature Fahrenheit
c2f (Temperature c) = Temperature (1.8 * c + 32)

k2c :: Temperature Kelvin -> Temperature Celsius
k2c (Temperature k) = Temperature $ k - 273.15

data Tree a
  = Nil
  | Branch (Tree a) a (Tree a)
  deriving (Eq, Show)

instance Functor Tree where
  fmap = fmapDefault

instance Foldable Tree where
  foldMap = foldMapDefault

instance Traversable Tree where
  traverse g Nil = pure Nil
  -- порядок зарешал, для этого сделан "flip"
  traverse g (Branch l x r) = liftA3 (\a b c -> Branch a c b) (traverse g l) (traverse g r) (g x)

testTree = Branch (Branch (Branch Nil 1 Nil) 2 (Branch Nil 3 Nil)) 4 (Branch Nil 5 Nil)

postOrderExample = foldMapDefault (\x -> [x]) testTree -- [1,3,2,5,4]

currentExample = postOrderExample
