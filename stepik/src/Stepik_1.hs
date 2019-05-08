module Stepik_1 where

import           Data.Char
import           Data.List (genericLength)

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
  | n < 1 = (-1) ^ (x + 1) * fibonacci x
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
--𝑎0=1; 𝑎1=2; 𝑎2=3; 𝑎𝑘+3=𝑎𝑘+2+𝑎𝑘+1−2𝑎𝑘
seqA' :: Integer -> Integer
seqA' 0 = 1
seqA' 1 = 2
seqA' 2 = 3
seqA' n = seqA' (n - 1) + seqA' (n - 2) - 2 * seqA' (n - 3)

seqA :: Integer -> Integer
seqA 0 = 1
seqA 1 = 2
seqA 2 = 3
seqA n = seqHelper 3 n 3 2 1
  where
    seqHelper current target a b c
      | current == target = a + b - 2 * c
      | otherwise = seqHelper (current + 1) target (a + b - 2 * c) a b

sumNCount :: Integer -> (Integer, Integer)
sumNCount x = (sum', count)
  where
    sum' = toInteger $ sum $ digitToInt <$> str
    count = genericLength str
    str = show $ abs x

--integration :: (Double -> Double) -> Double -> Double -> Double
--integration f a b = undefined
-- integration sin pi 0
-- -2.0
-- 2.3
class Printable a where
  toString :: a -> String

instance Printable Bool where
  toString True  = "true"
  toString False = "false"

instance Printable () where
  toString () = "unit type"

instance (Printable a, Printable b) => Printable (a, b) where
  toString (a, b) = "(" ++ toString a ++ "," ++ toString b ++ ")"

-- 2.4
class KnownToGork a where
  stomp :: a -> a
  doesEnrageGork :: a -> Bool

class KnownToMork a where
  stab :: a -> a
  doesEnrageMork :: a -> Bool

class (KnownToGork a, KnownToMork a) =>
      KnownToGorkAndMork a
  where
  stompOrStab :: a -> a
  stompOrStab a =
    case (doesEnrageMork a, doesEnrageGork a) of
      (True, False)  -> stomp a
      (False, True)  -> stab a
      (True, True)   -> stomp $ stab a
      (False, False) -> a

-- 2.4
ip = show a ++ show b ++ show c ++ show d
  where
    a = 127.22
    b = 4.1
    c = 20.1
    d = 2

class (Bounded a, Enum a, Eq a) =>
      SafeEnum a
  where
  ssucc :: a -> a
  ssucc x =
    if maxBound == x
      then minBound
      else succ x
  spred :: a -> a
  spred x =
    if minBound == x
      then maxBound
      else pred x

instance SafeEnum Bool

testValue = ssucc True

avg :: Int -> Int -> Int -> Double
avg a b c = (fromIntegral a + fromIntegral b + fromIntegral c) / 3

-- 3.1
nTimes :: a -> Int -> [a]
nTimes x n = loop x n []
  where
    loop y m acc
      | m == 0 = acc
      | otherwise = loop y (m - 1) (y : acc)

oddsOnly :: Integral a => [a] -> [a]
oddsOnly [] = []
oddsOnly (x:xs)
  | odd x = x : oddsOnly xs
  | otherwise = oddsOnly xs

sum2 :: Num a => [a] -> [a] -> [a]
sum2 (x:xs) (y:ys) = (x + y) : sum2 xs ys
sum2 xs []         = xs
sum2 [] ys         = ys
sum2 [] []         = []

sum3 :: Num a => [a] -> [a] -> [a] -> [a]
sum3 xs ys = sum2 $ sum2 xs ys
