{-# OPTIONS_GHC -fwarn-incomplete-patterns #-}

module Bits where

import           Data.Char
import           Data.List        (dropWhileEnd, foldl', genericLength, unfoldr)
import           Data.Time.Clock
import           Data.Time.Format

data Bit
  = Zero
  | One
  deriving (Eq, Show)

data Sign
  = Minus
  | Plus
  deriving (Eq, Show)

data Z =
  Z Sign [Bit]
  deriving (Eq, Show)

instance Ord Bit where
  compare Zero One = LT
  compare One Zero = GT
  compare _ _      = EQ

add :: Z -> Z -> Z
add (Z Plus xs) (Z Plus ys) = Z Plus (sumPositive xs ys)
add (Z Minus xs) (Z Minus ys) = Z Minus (sumPositive xs ys)
add (Z signX xs) (Z signY ys) = removeUnusedSign $ Z finalSign (subtractHiLow highest lowest)
  where
    finalSign =
      if compareBits xs ys == GT
        then signX
        else signY
    (lowest, highest) = minMax xs ys

mul :: Z -> Z -> Z
mul (Z _ _) (Z _ []) = Z Plus []
mul (Z _ []) (Z _ _) = Z Plus []
mul (Z signX xs) (Z signY ys) = Z (finalSign signX signY) (multiplyBits xs ys)
  where
    finalSign a b
      | a == b = Plus
      | otherwise = Minus
      -- TODO: no sign if empty

zipFill :: [Bit] -> [Bit] -> [(Bit, Bit)]
zipFill [] []         = []
zipFill (x:xs) []     = (x, Zero) : zipFill xs []
zipFill [] (y:ys)     = (Zero, y) : zipFill [] ys
zipFill (x:xs) (y:ys) = (x, y) : zipFill xs ys

removeUnusedSign :: Z -> Z
removeUnusedSign (Z _ []) = Z Plus []
removeUnusedSign z        = z

-- value, remainder
add3 :: Bit -> Bit -> Bit -> (Bit, Bit)
add3 Zero Zero Zero = (Zero, Zero) -- 0
add3 One One One    = (One, One) -- 3
add3 One Zero Zero  = (One, Zero) -- 1
add3 Zero One Zero  = (One, Zero) -- 1
add3 Zero Zero One  = (One, Zero) -- 1
add3 _ _ _          = (Zero, One) -- 2

-- остаток отнимается потом от разности.
-- это то, что нужно отнять от старшего разряда после прошлого действия.
-- новый остаток отходит к еще более старшему разряду.
sub3 :: Bit -> Bit -> Bit -> (Bit, Bit) -- high, low, remainer -> (value, remainder)
sub3 Zero Zero Zero = (Zero, Zero)
sub3 Zero One Zero  = (One, One) --
sub3 One Zero Zero  = (One, Zero)
sub3 One One Zero   = (Zero, Zero)
sub3 Zero Zero One  = (One, One)
sub3 Zero One One   = (Zero, One)
sub3 One Zero One   = (Zero, Zero)
sub3 One One One    = (One, One)

compareBits :: [Bit] -> [Bit] -> Ordering
compareBits xxs yys = uncurry compare . unzip . reverse $ zipFill xxs yys

minMax :: [Bit] -> [Bit] -> ([Bit], [Bit])
minMax xs ys =
  if compareBits xs ys == LT
    then (xs, ys)
    else (ys, xs)

-- TODO: заменить на рекурсию, как с фибоначчи.
sumPositive :: [Bit] -> [Bit] -> [Bit]
sumPositive xs ys = dropWhileEnd (== Zero) $ zzs ++ [lastreminder]
  where
    (zzs, lastreminder) = foldl' folder ([], Zero) $ zipFill xs ys
    folder (zs, rem) (x, y) = (zs ++ [fst (add3 x y rem)], snd $ add3 x y rem)

subtractHiLow :: [Bit] -> [Bit] -> [Bit]
subtractHiLow xs ys = dropWhileEnd (== Zero) . fst . foldl' folder ([], Zero) $ zipFill xs ys
  where
    folder (zs, rem) (x, y) = (zs ++ [fst (sub3 x y rem)], snd $ sub3 x y rem)

multiplyBits :: [Bit] -> [Bit] -> [Bit]
multiplyBits _ [] = []
multiplyBits [] _ = []
multiplyBits xs ys = sumPositive xs . multiplyBits xs $ dec ys
  where
    dec a = subtractHiLow a [One]

-- нужно сделать тестирование памятью и временем.
-- можно складывать вместе результаты умножения по разрядам
--
{-tests-}
emptyZ = Z Plus []

test001 = (add (Z Plus []) (Z Plus [])) == Z Plus []

test002 = (add (Z Plus []) (Z Plus [One])) == Z Plus [One]

test003 = (add (Z Plus []) (Z Minus [One])) == Z Minus [One]

test011 = (add (Z Plus [Zero, One, One]) (Z Plus [One])) == Z Plus [One, One, One]

test012 = (add (Z Plus [Zero, One, One]) (Z Plus [Zero, One])) == Z Plus [Zero, Zero, Zero, One]

test013 = (add (Z Plus [Zero, One, One]) (Z Plus [Zero, One, One])) == Z Plus [Zero, Zero, One, One]

test021 = (add (Z Minus [Zero, One, One]) (Z Minus [One])) == Z Minus [One, One, One]

test022 = (add (Z Minus [Zero, One, One]) (Z Minus [Zero, One])) == Z Minus [Zero, Zero, Zero, One]

test023 = (add (Z Minus [Zero, One, One]) (Z Minus [Zero, One, One])) == Z Minus [Zero, Zero, One, One]

test031 = (add (Z Minus [Zero, One, One]) (Z Plus [One])) == Z Minus [One, Zero, One]

test032 = (add (Z Minus [Zero, One, One]) (Z Plus [Zero, One])) == Z Minus [Zero, Zero, One]

test033 = (add (Z Minus [Zero, One, One]) (Z Plus [Zero, One, One])) == Z Plus []

test041 = (add (Z Plus [Zero, One, One]) (Z Minus [One])) == Z Plus [One, Zero, One]

test042 = (add (Z Plus [Zero, One, One]) (Z Minus [Zero, One])) == Z Plus [Zero, Zero, One]

test043 = (add (Z Plus [Zero, One, One]) (Z Minus [Zero, One, One])) == Z Plus []

test051 = (add (Z Plus [One]) (Z Minus [One])) == Z Plus []

test052 = (add (Z Plus [One]) (Z Minus [One, One])) == Z Minus [Zero, One]

test053 = (add (Z Plus [One]) (Z Minus [Zero, One])) == Z Minus [One]

test054 = (add (Z Plus [One]) (Z Minus [Zero, Zero, Zero, One])) == Z Minus [One, One, One]

test055 = (add (Z Plus [One]) (Z Minus [Zero, One, Zero, One])) == Z Minus [One, Zero, Zero, One]

test056 = (add (Z Plus [Zero, One]) (Z Minus [Zero, One, One])) == Z Minus [Zero, Zero, One]

test057 = (add (Z Plus [Zero, One]) (Z Minus [Zero, Zero, One])) == Z Minus [Zero, One]

test058 = (add (Z Plus [One, Zero, One]) (Z Minus [Zero, One, Zero, One])) == Z Minus [One, Zero, One]

test101 = (mul (Z Plus []) (Z Plus [])) == emptyZ

test102 = (mul (Z Plus []) (Z Plus [One])) == emptyZ

test103 = (mul (Z Plus []) (Z Minus [One])) == emptyZ

test104 = (mul (Z Plus [One]) (Z Plus [])) == emptyZ

test105 = (mul (Z Minus [One]) (Z Plus [])) == emptyZ

test111 = (mul (Z Plus [One]) (Z Plus [One])) == Z Plus [One]

test112 = (mul (Z Minus [One]) (Z Plus [One])) == Z Minus [One]

test113 = (mul (Z Plus [One]) (Z Minus [One])) == Z Minus [One]

test114 = (mul (Z Minus [One]) (Z Minus [One])) == Z Plus [One]

test121 = (mul (Z Plus [One]) (Z Plus [Zero, One])) == Z Plus [Zero, One]

test122 = (mul (Z Plus [Zero, Zero, One]) (Z Plus [Zero, Zero, One])) == Z Plus [Zero, Zero, Zero, Zero, One]

test131 =
  (mul (Z Plus [One, Zero, One, Zero, One]) (Z Plus [One, One, One])) ==
  Z Plus [One, One, Zero, Zero, One, Zero, Zero, One]

testAdd =
  test001 &&
  test002 &&
  test003 &&
  test011 &&
  test012 &&
  test013 &&
  test021 &&
  test022 &&
  test023 &&
  test031 &&
  test032 &&
  test033 &&
  test041 && test042 && test043 && test051 && test052 && test053 && test054 && test055 && test056 && test057 && test058

testMul =
  test101 &&
  test102 && test103 && test104 && test105 && test111 && test112 && test113 && test114 && test121 && test122 && test131

testAll = testAdd && testMul

--  print $ subtractHiLow (Z Plus [Zero, One, One]) (Z Plus [Zero, One, One])
testBits :: IO ()
testBits = do
  print $ multiplyBits [One, Zero, One, One] [One, Zero, One]
  print $ test001
  print $ test002
  print $ test003
  print $ test011
  print $ test012
  print $ test013
  print $ test021
  print $ test022
  print $ test023
  print $ test031
  print $ test032
  print $ test033
  print $ test041
  print $ test042
  print $ test043
  print $ test051
  print $ test052
  print $ test053
  print $ test054
  print $ test055
  print $ test056
  print $ test057
  print $ test058
  print $ test101
  print $ test102
  print $ test103
  print $ test104
  print $ test105
  print $ test111
  print $ test112
  print $ test113
  print $ test114
  print $ test121
  print $ test122
  print $ test131
