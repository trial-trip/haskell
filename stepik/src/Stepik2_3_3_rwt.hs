{-# LANGUAGE GeneralisedNewtypeDeriving #-}
{-# LANGUAGE RankNTypes                 #-}
{-# LANGUAGE TypeOperators              #-}

module Stepik2_3_3_rwt where

import           Control.Monad.Trans        (lift)
import           Control.Monad.Trans.Reader
import           Control.Monad.Trans.Writer
import           Data.Char                  (toUpper)
import           Data.List                  (partition)

type ExperimentT m a = ReaderT [String] m a

myLift' :: Monad m => m a -> ReaderT r m a
myLift' = lift

myAsks' :: Monad m => (r -> a) -> ReaderT r m a
myAsks' = asks

experimentFn :: ExperimentT IO String -- ReaderT [String] IO String
experimentFn = do
  el1 <- myAsks' head
  myLift' $ putStrLn $ "First is " ++ show el1
  el2 <- myAsks' (map toUpper . head . tail)
  myLift' $ putStrLn $ "Second is " ++ show el2
  return el2

experiment :: IO String
experiment = runReaderT experimentFn ["abc", "defg", "hij"] -- DEFG

type MyRWT m = ReaderT [String] (WriterT String m)

runMyRWT :: Monad m => MyRWT m a -> [String] -> m (a, String)
runMyRWT rw e = runWriterT (runReaderT rw e)

myLift :: Monad m => m a -> MyRWT m a
myLift = lift . lift

myAsks :: Monad m => ([String] -> a) -> MyRWT m a
myAsks = asks

myAsk :: Monad m => MyRWT m [String]
myAsk = asks id

myTell :: Monad m => String -> MyRWT m ()
myTell = lift . tell

logFirstAndRetSecond :: MyRWT IO String
logFirstAndRetSecond = do
  el1 <- myAsks head
  myLift $ putStrLn $ "First is " ++ show el1
  el2 <- myAsks (map toUpper . head . tail)
  myLift $ putStrLn $ "Second is " ++ show el2
  myTell el1
  return el2

complexExperiment = runMyRWT logFirstAndRetSecond ["abc", "defg", "hij"]
  --Stepik2_3_3_rwt.complexExperiment >>= print -- ("DEFG","abc")

logFirstAndRetSecond' :: MyRWT Maybe String
logFirstAndRetSecond' = do
  xs <- myAsk
  case xs of
    (el1:el2:_) -> myTell el1 >> return (map toUpper el2)
    _           -> myLift Nothing

safeExample1 = runMyRWT logFirstAndRetSecond' ["abc", "defg", "hij"] -- Just ("DEFG","abc")

safeExample2 = runMyRWT logFirstAndRetSecond' ["abc"] -- Nothing

veryComplexComputation :: MyRWT Maybe (String, String)
veryComplexComputation = do
  xs <- myAsks (uncurry zip . partition (even . length))
  case xs of
    ((a, b):(c, d):_) -> myTell (a ++ "," ++ b) >> return (toUpper <$> c, toUpper <$> d)
    _ -> myLift Nothing

veryComplexComputation1 = runMyRWT veryComplexComputation ["abc", "defg", "hij"] -- Nothing

veryComplexComputation2 = runMyRWT veryComplexComputation ["abc", "defg", "hij", "kl"] -- Just (("KL","HIJ"),"defg,abc")
