module Stepik2_4_2 where

import           Control.Monad.Trans        (lift)
import           Control.Monad.Trans.State
import           Control.Monad.Trans.Writer
import           Data.Monoid

data Tree a
  = Leaf a
  | Fork (Tree a) a (Tree a)
  deriving (Show)

go :: Tree () -> StateT Integer (Writer (Sum Integer)) (Tree Integer)
go tree =
  case tree of
    Leaf _ -> do
      x <- get
      modify succ
      lift . tell $ Sum 1
      return $ Leaf x
    Fork l _ r -> do
      leftSubtree <- go l
      central <- get
      modify succ
      rightSubtree <- go r
      return $ Fork leftSubtree central rightSubtree

numberAndCount :: Tree () -> (Tree Integer, Integer)
numberAndCount t = getSum <$> runWriter (evalStateT (go t) 1)

example1 = numberAndCount (Leaf ()) -- (Leaf 1,1)

example2 = numberAndCount (Fork (Leaf ()) () (Leaf ())) -- (Fork (Leaf 1) 2 (Leaf 3),2)
