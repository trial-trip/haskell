{-# OPTIONS_GHC -fwarn-incomplete-patterns #-}

module Stepik_1_monads where

import           Control.Applicative
import           Control.Monad         (ap, liftM)
import           Control.Monad.Writer
import           Data.Char             (isDigit)
import           Data.Functor.Identity
import           Data.List             (foldl', isInfixOf)
import           Data.Monoid
import           System.Directory      (getDirectoryContents, listDirectory)
import           System.IO             (hFlush, stdout)

data Log a =
  Log [String] a

toLogger :: (a -> b) -> String -> (a -> Log b)
toLogger f msg a = Log [msg] (f a)

execLoggers :: a -> (a -> Log b) -> (b -> Log c) -> Log c
execLoggers a f1 f2 =
  let (Log msg1 b) = f1 a
      (Log msg2 c) = f2 b
   in Log (msg1 ++ msg2) c

returnLog :: a -> Log a
returnLog = Log []

bindLog :: Log a -> (a -> Log b) -> Log b
bindLog (Log msg a) f =
  let (Log msg' b) = f a
   in Log (msg ++ msg') b

instance Functor Log where
  fmap = liftM

instance Applicative Log where
  pure = return
  (<*>) = ap

instance Monad Log where
  return = returnLog
  (>>=) = bindLog

execLoggersList :: a -> [a -> Log a] -> Log a
execLoggersList a = foldl' (>>=) (Log [] a)

data Token
  = Number Int
  | Plus
  | Minus
  | LeftBrace
  | RightBrace
  deriving (Eq, Show)

asToken :: String -> Maybe Token
asToken [] = Nothing
asToken "(" = Just LeftBrace
asToken ")" = Just RightBrace
asToken "-" = Just Minus
asToken "+" = Just Plus
asToken xs
  | all isDigit xs = Just $ Number $ read xs
  | otherwise = Nothing

tokenize :: String -> Maybe [Token]
tokenize = sequence . (<$>) asToken . words

--- board
data Board =
  Board (Int, Int) (Int, Int)
  deriving (Show)

nextPositions :: Board -> [Board]
nextPositions b = [b, b]

nextPositionsN :: Board -> Int -> (Board -> Bool) -> [Board]
nextPositionsN board n predicate
  | n < 0 = []
  | otherwise = filter predicate $ foldl (>>=) [board] (replicate n nextPositions)

pythagoreanTriple :: Int -> [(Int, Int, Int)]
pythagoreanTriple x
  | x <= 0 = []
  | otherwise = do
    a <- [1 .. x]
    b <- [1 .. x]
    c <- [1 .. x]
    if a ^ 2 + b ^ 2 == c ^ 2 && a < b
      then return (a, b, c)
      else []

main' :: IO ()
main' = do
  putStrLn "What is your name?"
  putStr "Name: "
  hFlush stdout
  name <- getLine
  if null name
    then main'
    else putStrLn $ "Hi, " ++ name ++ "!"

removeFile :: FilePath -> IO ()
removeFile name = putStrLn ("really removing ... " ++ name)

main'' :: IO ()
main'' = do
  putStr "Substring: "
  hFlush stdout
  name <- getLine
  if null name
    then putStrLn "Canceled"
    else do
      contents <- filter (isInfixOf name) <$> listDirectory "."
      mapM_ (\x -> putStrLn ("Removing file: " ++ x) >> removeFile x) contents

newtype Reader r a =
  Reader
    { runReader :: r -> a
    }

instance Functor (Reader r) where
  fmap = liftM

instance Applicative (Reader r) where
  pure = return
  (<*>) = ap

instance Monad (Reader r) where
  return x = Reader $ const x
  m >>= k = Reader $ \r -> runReader (k (runReader m r)) r

local' :: (r -> r') -> Reader r' a -> Reader r a
local' f m = Reader $ runReader m . f

type User = String

type Password = String

type UsersTable = [(User, Password)]

usersWithBadPasswords :: Reader UsersTable [User]
usersWithBadPasswords = Reader $ fmap fst . filter ((== "123456") . snd)

type Shopping = Writer ([String], Sum Integer) ()

purchase :: String -> Integer -> Shopping
purchase name price = tell ([name], Sum price)

shopping1 :: Shopping
shopping1 = do
  purchase "Jeans" 19200
  purchase "Water" 180
  purchase "Lettuce" 328

total :: Shopping -> Integer
total = getSum . snd . snd . runIdentity . runWriterT

items :: Shopping -> [String]
items = fst . snd . runIdentity . runWriterT

newtype State s a =
  State
    { runState :: s -> (a, s)
    }

readerToState :: Reader r a -> State r a
readerToState m = State $ \e -> (runReader m e, e)

writerToState :: Monoid w => Writer w a -> State w a
writerToState m =
  let (a, w) = runWriter m
   in State $ \e -> (a, mappend e w)

execState m s = snd (runState m s)

evalState m s = fst (runState m s)

fibStep :: State (Integer, Integer) ()
fibStep = State $ \(a, b) -> ((), (b, a + b))

execStateN :: Int -> State s a -> s -> s
execStateN n m = last . take n . iterate (execState m)

data Tree a
  = Leaf a
  | Fork (Tree a) a (Tree a)
  deriving (Show)

numTree :: Tree () -> Integer -> (Tree Integer, Integer)
numTree (Leaf _) n =
  let n' = succ n
   in (Leaf n', n')
numTree (Fork tree1 x tree2) n = (Fork (Leaf 42) 43 (Leaf 44), 100)

instance Functor Tree where
  fmap f (Leaf x)       = Leaf (f x)
  fmap f (Fork t1 x t2) = Fork (f <$> t1) (f x) (f <$> t2)

--numberTree :: Tree () -> Tree Integer
--numberTree t = fst $ numTree t 0
-- должна принимать инт?
--nTree :: State (Tree Integer) Integer
--nTree = do
--  let (t1, x, t2) = ask
--  a <- nTree
--  return (1, Leaf 1)

--numberTree :: Tree () -> Tree Integer
--numberTree t = execState nTree $ 5 <$ t
