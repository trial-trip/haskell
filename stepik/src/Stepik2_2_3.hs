{-# LANGUAGE GeneralisedNewtypeDeriving #-}
{-# LANGUAGE RankNTypes                 #-}
{-# LANGUAGE TypeOperators              #-}

module Stepik2_2_3 where

import           Control.Applicative (liftA2, liftA3)
import           Data.Foldable
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

cnt1 = Un 42

cnt3 = Bi 1 2 cnt1

cnt5 = Bi 3 4 cnt3

cntFunctor = (+ 1) <$> cnt5 -- Bi 4 5 (Bi 2 3 (Un 43))

cntFoldable1 = toList cnt5 -- [3,4,1,2,42]

cntFoldable2 = sum cnt5 -- 52

cntTraversable = traverse (\x -> [x + 2, x - 2]) cnt1 -- [Un 44,Un 40]

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
