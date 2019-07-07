{-# OPTIONS_GHC -fwarn-incomplete-patterns #-}

module Stepik_1_monads where

import           Control.Applicative
import           Control.Monad       (ap, liftM)
import           Data.Char           (isDigit)
import           Data.List           (foldl')
import           System.IO

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
