{-# OPTIONS_GHC -fwarn-incomplete-patterns #-}

module State_monad where

import           Control.Monad.State

data Tree a
  = Leaf a
  | Fork (Tree a) a (Tree a)
  deriving (Show)

step :: Tree () -> State Integer (Tree Integer)
step tree = do
  x <- get
  case tree of
    Leaf _ -> do
      put (x + 1)
      return $ Leaf (x + 1)
    Fork l _ r -> do
      leftSubtree <- step l
      x <- get
      put (x + 1)
      let central = x + 1
      rightSubtree <- step r
      return $ Fork leftSubtree central rightSubtree

numberTree :: Tree () -> Tree Integer
numberTree tree = evalState (step tree) 0

answer1 = numberTree $ Leaf () -- Leaf 1

answer2 = numberTree $ Fork (Leaf ()) () (Leaf ()) -- Fork (Leaf 1) 2 (Leaf 3)
