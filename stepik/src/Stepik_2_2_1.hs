{-# LANGUAGE RankNTypes    #-}
{-# LANGUAGE TypeOperators #-}

module Stepik_2_2_1 where

import           Control.Applicative (Alternative (..), ZipList (..),
                                      getZipList, liftA2, (<**>))
import           Data.Char           (isDigit)
import           Data.Function       (on, (&))
import           Data.List
import           Data.Monoid

data Triple a =
  Tr a a a
  deriving (Eq, Show)

instance Foldable Triple where
  foldl f ini (Tr a b c) = foldl f ini [a, b, c]
  foldr f ini (Tr a b c) = foldr f ini [a, b, c]

foldExample1 = foldr (++) "!!" (Tr "ab" "cd" "efg") -- "abcdefg!!"

foldExample2 = foldl (++) "!!" (Tr "ab" "cd" "efg") --"!!abcdefg"

data Tree a
  = Nil
  | Branch (Tree a) a (Tree a)
  deriving (Eq, Show)

instance Foldable Tree where
  foldr f ini Nil = ini
  foldr f ini (Branch t1 a t2) =
    let ini2 = foldr f ini t2
        ini3 = f a ini2
     in foldr f ini3 t1

newtype Preorder a =
  PreO (Tree a)
  deriving (Eq, Show)

instance Foldable Preorder where
  foldr f ini (PreO Nil) = ini
  foldr f ini (PreO (Branch t1 a t2)) =
    let ini2 = foldr f ini (PreO t2)
        ini3 = foldr f ini2 (PreO t1)
     in f a ini3

newtype Postorder a =
  PostO (Tree a)
  deriving (Eq, Show)

instance Foldable Postorder where
  foldr f ini (PostO Nil) = ini
  foldr f ini (PostO (Branch t1 a t2)) =
    let ini2 = f a ini
        ini3 = foldr f ini2 (PostO t2)
     in foldr f ini3 (PostO t1)

newtype Levelorder a =
  LevelO (Tree a)
  deriving (Eq, Show)

instance Foldable Levelorder where
  foldr f ini t = foldr f ini $ fmap snd $ sortBy (compare `on` fst) $ flatten 0 t

flatten :: Int -> Levelorder a -> [(Int, a)]
flatten lvl (LevelO (Branch l x r)) =
  let lvl' = succ lvl
   in flatten lvl' (LevelO l) ++ [(lvl', x)] ++ flatten lvl' (LevelO r)
flatten _ _ = []

tree = Branch (Branch Nil 1 (Branch Nil 2 Nil)) 3 (Branch Nil 4 Nil)

foldedTree1 = foldr (:) [] tree -- [1,2,3,4]

foldedTree2 = foldr (:) [] $ PreO tree -- [3,1,2,4]

foldedTree3 = foldr (:) [] $ PostO tree -- [2,1,4,3]

foldedTree4 = foldr (:) [] $ LevelO tree -- [3,1,4,2]

mkEndo :: Foldable t => t (a -> a) -> Endo a
mkEndo = foldMap Endo

mkEndoExample1 = appEndo (mkEndo [(+ 5), (* 3), (^ 2)]) 2 -- 17

mkEndoExample2 = appEndo (mkEndo (42, (* 3))) 2 -- 6

infixr 9 |.|

newtype (|.|) f g a =
  Cmps
    { getCmps :: f (g a)
    }
  deriving (Eq, Show)

instance (Foldable f, Foldable g) => Foldable ((|.|) f g) where
  foldMap fm = foldMap (foldMap fm) . getCmps

foldCmpsExample1 = maximum $ Cmps [Nothing, Just 2, Just 3] -- 3

foldCmpsExample2 = length $ Cmps [[1, 2], [], [3, 4, 5, 6, 7]] -- 7
