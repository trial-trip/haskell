{-# LANGUAGE GeneralisedNewtypeDeriving #-}
{-# LANGUAGE RankNTypes                 #-}
{-# LANGUAGE TypeOperators              #-}

module Stepik2_3_3 where

import           Control.Monad
import           Control.Monad.Trans        (lift)
import           Control.Monad.Trans.Except
import           Control.Monad.Trans.Reader
import           Control.Monad.Trans.State
import           Control.Monad.Trans.Writer
import           Data.Char                  (toUpper)

simplestTransformer :: ReaderT String (Writer String) String
simplestTransformer = do
  x <- asks id
  lift $ tell $ "writer, " ++ x
  return $ "reader, " ++ x

simplestTransformerExample = runWriter (runReaderT simplestTransformer "42") -- ("reader, 42","writer, 42")

simplestTransformer2 :: WriterT String (Reader String) String
simplestTransformer2 = do
  x <- lift $ asks id
  tell $ "writer, " ++ x
  return $ "reader, " ++ x

simplestTransformer2Example = runReader (runWriterT simplestTransformer2) "42" -- ("reader, 42","writer, 42")

strings = ["abc", "defg", "hij"]

secondElem :: Reader [String] String
secondElem = do
  el2 <- asks (map toUpper . head . tail)
  return el2

secondElemExample = runReader secondElem strings -- "DEFG"

logFirst :: [String] -> Writer String String
logFirst xs = do
  let el1 = head xs
  let el2 = (map toUpper . head . tail) xs
  tell el1
  return el2

logFirstExample = runWriter (logFirst strings)

type MyRW = ReaderT [String] (Writer String)

runMyRW :: MyRW a -> [String] -> (a, String)
runMyRW rw e = runWriter (runReaderT rw e)

myAsks :: ([String] -> a) -> MyRW a
myAsks = asks

myTell :: String -> MyRW ()
myTell = lift . tell

--logFirstAndRetSecond :: ReaderT [String] (Writer String) String
logFirstAndRetSecond :: MyRW String
logFirstAndRetSecond = do
  el1 <- asks head
  el2 <- asks (map toUpper . head . tail)
  lift $ tell el1
  return el2

--rwexample = runWriter (runReaderT logFirstAndRetSecond strings) -- ("DEFG","abc")
rwexample = runMyRW logFirstAndRetSecond strings -- ("DEFG","abc")

logFirstAndRetSecond' :: WriterT String (Reader [String]) String
logFirstAndRetSecond' = do
  el1 <- lift $ asks head
  el2 <- lift $ asks (map toUpper . head . tail)
  tell el1
  return el2

rwexample' = runReader (runWriterT logFirstAndRetSecond') strings -- ("DEFG","abc")

separate :: (a -> Bool) -> (a -> Bool) -> [a] -> WriterT [a] (Writer [a]) [a]
separate p1 p2 xs = do
  let xs1 = filter p1 xs
  let xs2 = filter p2 xs
  let xs3 = filter (\x -> not (p1 x || p2 x)) xs
  tell xs1
  lift $ tell xs2
  return xs3

separateExample = (runWriter . runWriterT) $ separate (< 3) (> 7) [0 .. 10] -- (([3,4,5,6,7],[0,1,2]),[8,9,10])

tickCollatz :: State Integer Integer
tickCollatz = do
  n <- get
  let res =
        if odd n
          then 3 * n + 1
          else n `div` 2
  put res
  return n

tickCollatzExample = runState tickCollatz 2 -- (2, 1)

type EsSi a = ExceptT String (State Integer) a

-- решение далеко от идеала. Неоптимально использует lift, не использует when и throwE.
go :: Integer -> Integer -> State Integer Integer -> EsSi ()
go lb ub tc = do
  x <- lift get
  let z = execState tc x
  lift $ put z
  let res =
        case z of
          n
            | n <= lb -> Left "Lower bound"
          n
            | n >= ub -> Left "Upper bound"
          _ -> Right ()
  ExceptT (return res) -- except res тупит на Stepik

runEsSi :: EsSi a -> Integer -> (Either String a, Integer)
runEsSi = runState . runExceptT

runEsSi1 = runEsSi (go 1 85 tickCollatz) 27 -- (Right (),82)

runEsSi2 = runEsSi (go 1 80 tickCollatz) 27 -- (Left "Upper bound",82)

runEsSi3 :: (Either String Integer, Integer)
runEsSi3 = runEsSi (forever $ go 1 1000 tickCollatz) 27 -- (Left "Upper bound",1186)

runEsSi4 :: (Either String Integer, Integer)
runEsSi4 = runEsSi (forever $ go 1 10000 tickCollatz) 27 -- (Left "Lower bound",1)
