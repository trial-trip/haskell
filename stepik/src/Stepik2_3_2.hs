{-# LANGUAGE GeneralisedNewtypeDeriving #-}
{-# LANGUAGE RankNTypes                 #-}
{-# LANGUAGE TypeOperators              #-}

module Stepik2_3_2 where

import           Control.Applicative
import           Control.Monad
import           Control.Monad.Cont
import           Control.Monad.Except
import           Stepik2_3_1          (ReadError, tryRead)

decode c = c 0

add :: Int -> Int -> (Int -> r) -> r
add x y c = c $ x + y

multiply :: Int -> Int -> (Int -> r) -> r
multiply x y c = c $ x * y

cpsExample0 = decode (add 1) (multiply 100) (add 20) (add 1) id

type Checkpointed a = forall r. (a -> Cont r a) -> Cont r a

addTens :: Int -> Checkpointed Int
addTens x1 checkpoint = do
  checkpoint x1
  let x2 = x1 + 10
  checkpoint x2 {- x2 = x1 + 10 -}
  let x3 = x2 + 10
  checkpoint x3 {- x3 = x1 + 20 -}
  let x4 = x3 + 10
  return x4 {- x4 = x1 + 30 -}

runCheckpointed :: (a -> Bool) -> Checkpointed a -> a
runCheckpointed predicate mfn = runCont (mfn checker) id
  where
    checker x =
      cont $ \c ->
        if predicate $ c x
          then c x
          else x

checkpointedExample1 = runCheckpointed (< 100) $ addTens 1 --31

checkpointedExample2 = runCheckpointed (< 30) $ addTens 1 --21

checkpointedExample3 = runCheckpointed (< 20) $ addTens 1 --11

checkpointedExample4 = runCheckpointed (< 10) $ addTens 1 --1

newtype FailCont r e a =
  FailCont
    { runFailCont :: (a -> r) -> (e -> r) -> r
    }

instance Functor (FailCont r e) where
  fmap = liftM

instance Applicative (FailCont r e) where
  pure = return
  (<*>) = ap

instance Monad (FailCont r e) where
  return x = FailCont $ \ok err -> ok x
  m >>= k = FailCont $ \ok err -> runFailCont m (\a -> runFailCont (k a) ok err) err
  -- Cont v >>= k = Cont $ \c -> v (\a -> runCont (k a) c)

toFailCont :: Except e a -> FailCont r e a
toFailCont m = FailCont $ \ok err -> either err ok $ runExcept m

evalFailCont :: FailCont (Either e a) e a -> Either e a
evalFailCont m = runFailCont m Right Left

addx :: Int -> Int -> FailCont r e Int
addx x y = FailCont $ \ok _ -> ok $ x + y

addFail :: Int -> Int -> FailCont r String Int
addFail x y = FailCont $ \_ err -> err "Such error"

failContExperiment1 :: Int
failContExperiment1 = runFailCont (addx 5 3) id id -- 8

failContExperiment2 :: String
failContExperiment2 = runFailCont (addFail 5 3) show id -- "Fuck it"

addInts :: String -> String -> FailCont r ReadError Int
addInts s1 s2 = do
  i1 <- toFailCont $ tryRead s1
  i2 <- toFailCont $ tryRead s2
  return $ i1 + i2

failContExperiment3 = evalFailCont $ addInts "15" "12" -- Right 27

failContExperiment4 = runFailCont (addInts "15" "") print (putStrLn . ("Oops: " ++) . show) --Oops: EmptyInput

callCFC :: ((a -> FailCont r e b) -> FailCont r e a) -> FailCont r e a
callCFC f = FailCont $ \ok err -> runFailCont (f $ \a -> FailCont $ \_ _ -> ok a) ok err
