module Stepik_1 where

import           Data.Char

lenVec3 :: Float -> Float -> Float -> Float
lenVec3 a b c = sqrt $ a ^ 2 + b ^ 2 + c ^ 2

lenVec3' :: Float -> Float -> Float -> Float
lenVec3' a b c = sqrt $ sqrt (a ^ 2 + b ^ 2) ^ 2 + c ^ 2

-- 1.3
sign 0 = 0
sign x =
  if x < 0
    then (-1)
    else 1

x |-| y = diffMod
  where
    diffMod =
      if diff >= 0
        then diff
        else (-diff)
    diff = x - y

-- 1.4
twoDigits2Int :: Char -> Char -> Int
twoDigits2Int x y =
  if isDigit x && isDigit y
    then read [x, y] :: Int
    else 100

dist :: (Double, Double) -> (Double, Double) -> Double
dist (x1, y1) (x2, y2) = sqrt $ (x1 - x2) ^ 2 + (y1 - y2) ^ 2

-- 1.5
doubleFact :: Integer -> Integer
doubleFact n = product [x | x <- [1 .. n], cN == x `mod` 2]
  where
    cN = n `mod` 2

doubleFact' :: Integer -> Integer
doubleFact' 0 = 1
doubleFact' 1 = 1
doubleFact' n = n * doubleFact' (n - 2)

fibonacci :: Integer -> Integer
fibonacci 0 = 0
fibonacci 1 = 1
fibonacci n
  | n > 1 = fibonacci (n - 1) + fibonacci (n - 2)
  | n < 1 = (-1) ^ (x + 1) * fibonacci (x)
  where
    x = -n

-- calculates positive cases
fibHelper :: Integer -> Integer -> Integer -> Integer -> Integer
fibHelper current target a b
  | target == 0 = 0
  | target == 1 = 1
  | current == target = a + b
  | otherwise = fibHelper (current + 1) target b (a + b)

-- calculates all cases in terms of positive
fibonacci' :: Integer -> Integer
fibonacci' n
  | n >= 0 = fibHelper 2 n 0 1
  | n < 1 = (-1) ^ (-n + 1) * fibonacci' (-n)

-- 1.6
--𝑎0=1;𝑎1=2;𝑎2=3;𝑎𝑘+3=𝑎𝑘+2+𝑎𝑘+1−2𝑎𝑘
seqA' :: Integer -> Integer
seqA' 0 = 1
seqA' 1 = 2
seqA' 2 = 3
seqA' n = (seqA' (n - 1)) + (seqA' (n - 2)) - 2 * (seqA' (n - 3))

seqA :: Integer -> Integer
seqA 0 = 1
seqA 1 = 2
seqA 2 = 3
seqA n = (seqA (n - 1)) + (seqA (n - 2)) - 2 * (seqA (n - 3))

