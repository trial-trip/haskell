{-# LANGUAGE GeneralisedNewtypeDeriving #-}
{-# LANGUAGE RankNTypes                 #-}
{-# LANGUAGE TypeOperators              #-}

module Stepik2_3_3_essi_hard where

import           Control.Monad
import           Control.Monad.Trans          (lift)
import           Control.Monad.Trans.Except
import           Control.Monad.Trans.Identity
import           Control.Monad.Trans.Reader
import           Control.Monad.Trans.State
import           Control.Monad.Trans.Writer
import           Data.Char                    (toUpper)

tickCollatz' :: StateT Integer IO Integer
tickCollatz' = do
  n <- get
  let res =
        if odd n
          then 3 * n + 1
          else n `div` 2
  lift $ print res
  put res
  return n

type RiiEsSiT m a = ReaderT (Integer, Integer) (ExceptT String (StateT Integer m)) a

go' :: Monad m => StateT Integer m Integer -> RiiEsSiT m ()
go' tc = do
  lift . lift $ tc
  z <- lift . lift $ get
  (lb, ub) <- ask
  when (z <= lb) (lift $ throwE "Lower bound")
  when (z >= ub) (lift $ throwE "Upper bound")

runRiiEsSiT ::
     ReaderT (Integer, Integer) (ExceptT String (StateT Integer m)) a
  -> (Integer, Integer)
  -> Integer
  -> m (Either String a, Integer)
runRiiEsSiT m = runStateT . runExceptT . runReaderT m
  --step1 ::
  --     ReaderT (Integer, Integer) (ExceptT String (StateT Integer m)) a
  --  -> (Integer, Integer)
  --  -> (ExceptT String (StateT Integer m)) a
  --step1 m bounds = runReaderT m bounds
  --step2 :: (ExceptT String (StateT Integer m)) a -> StateT Integer m (Either String a)
  --step2 m = runExceptT m
  --step3 :: StateT Integer m (Either String a) -> Integer -> m (Either String a, Integer)
  --step3 m i = runStateT m i

hardExample :: IO (Either String Integer, Integer)
hardExample = runRiiEsSiT (forever $ go' tickCollatz') (1, 200) 27
  --82
  --41
  --124
  --62
  --31
  --94
  --47
  --142
  --71
  --214
  --(Left "Upper bound",214)
