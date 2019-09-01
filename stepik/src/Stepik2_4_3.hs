module Stepik2_4_3 where

import           Control.Applicative
import           Control.Monad
import           Control.Monad.IO.Class     (liftIO)
import           Control.Monad.Trans        (lift)
import           Control.Monad.Trans.Except
import           Control.Monad.Trans.Maybe
import           Control.Monad.Trans.State
import           Control.Monad.Trans.Writer

import           Data.Char                  (isNumber, isPunctuation)
import           Data.Either                (lefts)
import           Data.Foldable              (traverse_)
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

newtype PwdError =
  PwdError String
  deriving (Show)

instance Semigroup PwdError where
  a <> b = a

instance Monoid PwdError where
  mempty = PwdError ""

type PwdErrorMonad = ExceptT PwdError IO

askPassword :: PwdErrorMonad ()
askPassword = do
  liftIO $ putStrLn "Enter your new password:"
  value <- msum $ repeat getValidPassword
  liftIO $ putStrLn "Storing in database..."

getValidPassword :: PwdErrorMonad String
getValidPassword = do
  s <- liftIO getLine
  validate s

validate :: String -> PwdErrorMonad String
validate s
  | length s < 8 = printError "Incorrect input: password is too short!"
  | not $ any isNumber s = printError "Incorrect input: password must contain some digits!"
  | not $ any isPunctuation s = printError "Incorrect input: password must contain some punctuation!"
  | otherwise = return s

printError :: String -> PwdErrorMonad String
printError err = do
  lift $ putStrLn err
  throwE $ PwdError err

askPasswordExample = runExceptT askPassword
  {-
  runExceptT askPassword
  Enter your new password:
  qwerty
  Incorrect input: password is too short!
  qwertyuiop
  Incorrect input: password must contain some digits!
  qwertyuiop123
  Incorrect input: password must contain some punctuation!
  qwertyuiop123!!!
  Storing in database...
  -}

data ReadError
  = EmptyInput
  | NoParse String
  deriving (Show)

-- моя реализация работает без изменений
tryRead :: (Read a, Monad m) => String -> ExceptT ReadError m a
tryRead [] = except $ Left EmptyInput
tryRead str =
  except $
  case reads str of
    ((x, ""):_) -> Right x
    _           -> Left $ NoParse str

data Tree a
  = Leaf a
  | Fork (Tree a) a (Tree a)
  
--instance Foldable Tree where

--стоит ли закладывать order в инстанс?
-- заново реализовать, потому что другие конструкторы.

treeSum :: Tree a -> (Maybe ReadError, Integer)
treeSum t =
  let (err, s) = runWriter . runExceptT $ traverse_ go t
   in (maybeErr err, getSum s)
  where
    maybeErr :: Either ReadError () -> Maybe ReadError
    maybeErr = either Just (const Nothing)

go :: String -> ExceptT ReadError (Writer (Sum Integer)) ()
go = undefined

inOrderExample1 = treeSum $ Fork (Fork (Leaf "1") "2" (Leaf "oops")) "15" (Leaf "16")
  --(Just (NoParse "oops"),3)

inOrderExample2 = treeSum $ Fork (Fork (Leaf "1") "2" (Leaf "0")) "15" (Leaf "16")
  --(Nothing,34)
