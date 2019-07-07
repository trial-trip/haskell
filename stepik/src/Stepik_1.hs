{-# OPTIONS_GHC -fwarn-incomplete-patterns #-}

module Stepik_1 where

import           Control.Applicative
import           Data.Char
import           Data.Function       ((&))
import           Data.List           (dropWhileEnd, foldl', genericLength,
                                      unfoldr)
import qualified Data.List           as L
import           Data.List.Split     (splitOn)
import           Data.Time.Clock
import           Data.Time.Format

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

sum3 :: Num a => [a] -> [a] -> [a] -> [a]
sum3 xs ys = sum2 $ sum2 xs ys

groupElems :: Eq a => [a] -> [[a]]
groupElems xs = reverse $ loop [] xs
  where
    loop acc@(accHead:accTail) (x:xs)
      | head accHead == x = loop ((x : accHead) : accTail) xs
      | otherwise = loop ([x] : acc) xs
    loop [] (x:xs) = loop [[x]] xs
    loop acc [] = acc

qsort :: Ord a => [a] -> [a]
qsort []     = []
qsort (x:xs) = qsort (filter (< x) xs) ++ [x] ++ qsort (filter (>= x) xs)

-- permutations of Eq a
permsEq :: Eq a => [a] -> [[a]]
permsEq []  = [[]]
permsEq [x] = [[x]]
permsEq xs  = concatMap (\x -> map ((:) x) $ permsEq (filter (/= x) xs)) xs

-- permutations of elements without Eq constraint (assuming their uniqueness)
perms :: [a] -> [[a]]
perms [] = [[]]
perms xs = concatMap (\(z:zs) -> map (z :) (perms zs)) $ map (uncurry offset) $ zip [0 ..] $ map (const xs) xs

offset :: Int -> [a] -> [a]
offset 0 xs     = xs
offset n []     = []
offset n (x:xs) = offset (n - 1) xs ++ [x]

fibStream :: [Integer]
fibStream = 0 : 1 : zipWith (+) fibStream (tail fibStream)

data Odd =
  Odd Integer
  deriving (Eq, Show)

instance Enum Odd where
  succ (Odd x) = Odd (x + 2)
  pred (Odd x) = Odd (x - 2)
  enumFrom x = x : enumFrom (succ x)
  enumFromTo oddX@(Odd x) oddY@(Odd y)
    | x > y = []
    | x == y = [oddX]
    | x < y = oddX : enumFromTo (succ oddX) oddY
  enumFromThen oddX@(Odd x) oddY@(Odd y) = oddX : enumFromThen oddY (Odd (y + y - x))
  enumFromThenTo oddX@(Odd x) oddY@(Odd y) oddZ@(Odd z) =
    if x < y
      then enumFromThenTo1 oddX oddY oddZ
      else enumFromThenTo2 oddX oddY oddZ
    where
      enumFromThenTo1 oddX@(Odd x) oddY@(Odd y) oddZ@(Odd z)
        | x > z = []
        | x == z = [oddX]
        | x < y = oddX : enumFromThenTo oddY (Odd (y + y - x)) oddZ
      enumFromThenTo2 oddX@(Odd x) oddY@(Odd y) oddZ@(Odd z)
        | x < z = []
        | x == z = [oddX]
        | x > y = oddX : enumFromThenTo oddY (Odd (y + y - x)) oddZ
  fromEnum (Odd x) = fromIntegral x
  toEnum x
    | odd x = Odd (toInteger x)
    | otherwise = error "non-odd value"

coins :: (Ord a, Num a) => [a]
coins = [2, 3, 7]

change :: (Ord a, Num a) => a -> [[a]]
change 0 = [[]]
change n = [coin : rest | coin <- coins, n >= coin, rest <- change (n - coin)] -- with sugar

--change n = filter (n >= ) coins >>= (\coin -> map (coin:) (change (n - coin))) -- without sugar
meanList :: [Double] -> Double
meanList xs = foldr (+) 0 xs / fromIntegral (length xs)

evenOnly' :: [a] -> [a]
evenOnly' =
  fst .
  foldl'
    (\(acc, index) x ->
       ( if odd index
           then acc ++ [x]
           else acc
       , succ index))
    ([], 0)

evenOnly :: [a] -> [a]
--evenOnly zs = map snd . filter (even . fst) . zip [1..] $ zs
--evenOnly = foldr (\pair rest -> if even $ fst pair then snd pair : rest else rest) [] . zip [1..]
evenOnly (_:x:xs) = x : evenOnly xs
evenOnly _        = []

revRange :: (Char, Char) -> String
revRange = unfoldr g
  where
    g (a, z)
      | a <= z = Just (z, (a, pred z))
      | otherwise = Nothing

timeToString :: UTCTime -> String
timeToString = formatTime defaultTimeLocale "%a %d %T"

data LogLevel
  = Error
  | Warning
  | Info
  deriving (Show)

data LogEntry =
  LogEntry
    { timestamp :: UTCTime
    , logLevel  :: LogLevel
    , message   :: String
    }

logLevelToString :: LogLevel -> String
logLevelToString = show

logEntryToString :: LogEntry -> String
logEntryToString (LogEntry timestamp logLevel message) =
  timeToString timestamp ++ ": " ++ show logLevel ++ ": " ++ message

data Coord a =
  Coord a a
  deriving (Eq, Show)

distance :: Coord Double -> Coord Double -> Double
distance (Coord x1 y1) (Coord x2 y2) = sqrt $ (x2 - x1) ^ 2 + (y2 - y1) ^ 2

manhDistance :: Coord Int -> Coord Int -> Int
manhDistance (Coord x1 y1) (Coord x2 y2) = abs (x2 - x1) + abs (y2 - y1)

getCenter :: Double -> Coord Int -> Coord Double
getCenter width (Coord x y) = Coord (width * (fromIntegral x + 0.5)) (width * (fromIntegral y + 0.5))

getCell :: Double -> Coord Double -> Coord Int
getCell width (Coord x y) = Coord (floor $ x / width) (floor $ y / width)

data Error
  = ParsingError
  | IncompleteDataError
  | IncorrectDataError String
  deriving (Show)

data Person =
  Person
    { firstName :: String
    , lastName  :: String
    , age       :: Int
    }
  deriving (Show)

trim = dropWhileEnd isSpace . dropWhile isSpace

safeTail :: [a] -> Maybe [a]
safeTail []     = Nothing
safeTail (_:xs) = Just xs

parseLine :: String -> Either Error (String, String)
parseLine str =
  case (trim $ takeWhile (/= '=') str, fmap trim (safeTail $ dropWhile (/= '=') str)) of
    (a, Just b)
      | not (null a || null b) -> Right (a, b)
    _ -> Left ParsingError

maybeToEither :: a -> Maybe b -> Either a b
maybeToEither _ (Just b) = Right b
maybeToEither a _        = Left a

parseAge :: String -> Either Error Int
parseAge str =
  if all isDigit str
    then Right $ read str
    else Left $ IncorrectDataError str

parsePerson :: String -> Either Error Person
parsePerson str = sequence (fmap parseLine (lines str)) >>= lookupPerson
  where
    lookupPerson entries =
      liftA3
        Person
        (maybeToEither IncompleteDataError $ lookup "firstName" entries)
        (maybeToEither IncompleteDataError $ lookup "lastName" entries)
        (maybeToEither IncompleteDataError (lookup "age" entries) >>= parseAge)

data Tree a
  = Leaf a
  | Node (Tree a) (Tree a)

height :: Tree a -> Int
height (Leaf _)   = 0
height (Node a b) = 1 + max (height a) (height b)

size :: Tree a -> Int
size (Leaf _)   = 1
size (Node a b) = 1 + size a + size b

avg' :: Tree Int -> Int
avg' t =
  let (c, s) = go t
   in s `div` c
  where
    go :: Tree Int -> (Int, Int)
    go (Leaf a) = (1, a)
    go (Node t1 t2) =
      let (q1, s1) = go t1
          (q2, s2) = go t2
       in (q1 + q2, s1 + s2)

-- rename: lookup2, empty2
class MapLike m where
  empty2 :: m k v
  lookup2 :: Ord k => k -> m k v -> Maybe v
  insert :: Ord k => k -> v -> m k v -> m k v
  delete :: Ord k => k -> m k v -> m k v
  fromList :: Ord k => [(k, v)] -> m k v
  fromList []          = empty2
  fromList ((k, v):xs) = insert k v (fromList xs)

newtype ListMap k v =
  ListMap
    { getListMap :: [(k, v)]
    }
  deriving (Eq, Show)

instance MapLike ListMap where
  empty2 = ListMap []
  lookup2 k = fmap snd . L.find ((== k) . fst) . getListMap
  delete k (ListMap xs) = ListMap $ filter ((/= k) . fst) xs
  insert k v m@(ListMap xs) =
    case lookup2 k m of
      Just _ ->
        ListMap $
        map
          (\(kk, vv) ->
             if kk == k
               then (kk, v)
               else (kk, vv))
          xs
      Nothing -> ListMap $ (k, v) : xs

newtype ArrowMap k v =
  ArrowMap
    { getArrowMap :: k -> Maybe v
    }

instance MapLike ArrowMap where
  empty2 = ArrowMap (const Nothing)
  lookup2 = flip getArrowMap
  insert k v m =
    ArrowMap
      (\x ->
         if x == k
           then Just v
           else lookup2 x m)
  delete k m =
    ArrowMap
      (\x ->
         if x == k
           then Nothing
           else lookup2 x m)
  fromList []          = empty2
  fromList ((a, b):xs) = insert a b (fromList xs)

testMap :: ArrowMap Int String
testMap = fromList [(1, "one"), (2, "two"), (3, "three")]
