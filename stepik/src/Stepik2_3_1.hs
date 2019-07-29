{-# LANGUAGE GeneralisedNewtypeDeriving #-}
{-# LANGUAGE RankNTypes                 #-}
{-# LANGUAGE TypeOperators              #-}

module Stepik2_3_1 where

import           Control.Applicative
import           Control.Monad
import           Control.Monad.Trans.Except

data ListIndexError
  = ErrIndexTooLarge Int
  | ErrNegativeIndex
  deriving (Eq, Show)

safeAt :: [a] -> Int -> Int -> Except ListIndexError a
safeAt [] target current = except $ Left $ ErrIndexTooLarge target
safeAt (x:xs) target current
  | target == current = except $ Right x
  | otherwise = safeAt xs target (succ current)

infixl 9 !!!

(!!!) :: [a] -> Int -> Except ListIndexError a
(!!!) xs n
  | n < 0 = except $ Left ErrNegativeIndex
  | otherwise = safeAt xs n 0

(!!!!) xs n = runExcept $ xs !!! n

safeAtExample1 = runExcept $ [1 .. 100] !!! 5 --Right 6

safeAtExample2 = [1, 2, 3] !!!! 0 --Right 1

safeAtExample3 = [1, 2, 3] !!!! 42 --  Left (ErrIndexTooLarge 42)

safeAtExample4 = [1, 2, 3] !!!! (-3) --  Left ErrNegativeIndex

data ReadError
  = EmptyInput
  | NoParse String
  deriving (Show)

tryRead [] = except $ Left EmptyInput
tryRead str =
  except $
  case reads str of
    ((x, ""):_) -> Right x
    _           -> Left $ NoParse str

tryReadExample1 = runExcept (tryRead "5" :: Except ReadError Int) -- Right 5

tryReadExample2 = runExcept (tryRead "5" :: Except ReadError Double) -- Right 5.0

tryReadExample3 = runExcept (tryRead "5zzz" :: Except ReadError Int) -- Left (NoParse "5zzz")

tryReadExample4 = runExcept (tryRead "(True, ())" :: Except ReadError (Bool, ())) -- Right (True,())

tryReadExample5 = runExcept (tryRead "" :: Except ReadError (Bool, ())) -- Left EmptyInput

tryReadExample6 = runExcept (tryRead "wrong" :: Except ReadError (Bool, ())) -- Left (NoParse "wrong")

data SumError =
  SumError Int ReadError
  deriving (Show)

trySum :: [String] -> Except SumError Integer
trySum =
  foldl
    (\ma (i, x) -> do
       a <- ma
       y <- withExcept (SumError i) $ tryRead x
       return $ a + y)
    (return 0) .
  zip [1 ..] -- на самом деле можно было через sequence

trySumExample :: Except SumError Integer
trySumExample = trySum ["1", "2", "r", "4"]

trySumExample1 = runExcept $ trySum ["10", "20", "30"] -- Right 60

trySumExample2 = runExcept $ trySum ["10", "20", ""] -- Left (SumError 3 EmptyInput)

trySumExample3 = runExcept $ trySum ["10", "two", "30"] -- Left (SumError 2 (NoParse "two"))

newtype SimpleError =
  Simple
    { getSimple :: String
    }
  deriving (Eq, Show)

instance Semigroup SimpleError where
  a <> b = Simple $ getSimple a ++ getSimple b

instance Monoid SimpleError where
  mempty = Simple ""

lie2se :: ListIndexError -> SimpleError
lie2se ErrNegativeIndex = Simple "[negative index]"
lie2se (ErrIndexTooLarge n) = Simple $ "[index (" ++ show n ++ ") is too large]"

newtype Validate e a =
  Validate
    { getValidate :: Either [e] a
    }

instance Functor (Validate e) where
  fmap f = Validate . fmap f . getValidate

instance Applicative (Validate e) where
  pure = Validate . Right
  v1 <*> v2 =
    Validate $
    case (getValidate v1, getValidate v2) of
      (Left xs, Left ys) -> Left $ xs ++ ys
      (Right _, Left ys) -> Left ys
      (Left xs, Right _) -> Left xs
      (Right f, Right x) -> Right $ f x

collectE :: Except e a -> Validate e a
collectE = Validate . runExcept . withExcept (: [])

validateSum :: [String] -> Validate SumError Integer
validateSum xs =
  foldl (\ma (i, x) -> (+) <$> ma <*> (collectE . withExcept (SumError i) . tryRead $ x)) (pure 0) $ zip [1 ..] xs

validateSumExample1 = getValidate $ validateSum ["10", "20", "30"] -- Right 60

validateSumExample2 = getValidate $ validateSum ["10", "", "30", "oops"] -- Left [SumError 2 EmptyInput,SumError 4 (NoParse "oops")]
