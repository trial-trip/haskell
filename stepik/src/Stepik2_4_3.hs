module Stepik2_4_3 where

import           Control.Monad.Trans        (lift)
import           Control.Monad.Trans.Except
import           Control.Monad.Trans.State
import           Control.Monad.Trans.Writer
import           Data.Either                (lefts)
import           Data.Monoid

data Tile
  = Floor
  | Chasm
  | Snake
  deriving (Show)

data DeathReason
  = Fallen
  | Poisoned
  deriving (Eq, Show)

type Point = (Integer, Integer)

type GameMap = Point -> Tile

-- clockwise
move :: GameMap -> Point -> ExceptT DeathReason [] Point
move m (x, y) = ExceptT $ verify <$> [(x, pred y), (succ x, y), (x, succ y), (pred x, y)]
  where
    verify p =
      case m p of
        Floor -> Right p
        Chasm -> Left Fallen
        Snake -> Left Poisoned

moves :: GameMap -> Int -> Point -> [Either DeathReason Point]
moves gmap n startPoint = runExceptT . foldl (>>=) (return startPoint) . replicate n $ move gmap

waysToDie :: DeathReason -> GameMap -> Int -> Point -> Int
waysToDie reason gmap n startPoint = length . filter (== reason) . lefts $ moves gmap n startPoint

{-
 | 0 1 2 3 4 5
--------------
0| o o o o o o
1| o       s o
2| o   s     o
3| o         o
4| o         o
5| o o o o o o
-}
map1 :: GameMap
map1 (2, 2) = Snake
map1 (4, 1) = Snake
map1 (x, y)
  | 0 < x && x < 5 && 0 < y && y < 5 = Floor
  | otherwise = Chasm

example1 = waysToDie Poisoned map1 1 (4, 2)
  --  1  -- можно пойти к змее наверх

example2 = waysToDie Poisoned map1 2 (4, 2)
  --  2  -- можно пойти к змее наверх или к змее влево

example3 = waysToDie Poisoned map1 3 (4, 2)
  --  5  -- за три шага к левой змее, по-прежнему можно дойти одним способом,
  --     -- а к правой — уже четырьмя (вверх, влево-вверх-вправо, влево-вправо-вверх, вниз-вверх-вверх)

example4 = waysToDie Poisoned map1 4 (4, 2)
  --  13
