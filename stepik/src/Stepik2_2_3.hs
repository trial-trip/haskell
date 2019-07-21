{-# LANGUAGE RankNTypes    #-}
{-# LANGUAGE TypeOperators #-}

module Stepik2_2_3 where

import           Control.Applicative (liftA2, liftA3)
import           Data.Foldable

-- Сделайте этот тип данных представителем классов типов Functor, Foldable и Traversable:
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

currentExample = cntTraversable
