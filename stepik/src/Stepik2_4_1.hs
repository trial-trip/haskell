{-# LANGUAGE InstanceSigs #-}

module Stepik2_4_1 where

import           Control.Applicative        (liftA2)
import qualified Control.Monad.Fail         as Fail
import           Control.Monad.Trans        (lift)
import           Control.Monad.Trans.Class
import           Control.Monad.Trans.Reader
import           Control.Monad.Trans.State
import           Control.Monad.Trans.Writer
import           Data.Char                  (toUpper)
import           Data.Functor.Identity
import           Data.List                  (partition)

data Logged a =
  Logged String a
  deriving (Eq, Show)

instance Functor Logged where
  fmap f (Logged str a) = Logged str $ f a

instance Applicative Logged where
  pure = Logged ""
  (Logged str1 f) <*> (Logged str2 a) = Logged (str1 ++ str2) $ f a

newtype LoggT m a =
  LoggT
    { runLoggT :: m (Logged a)
    }

instance Monad m => Functor (LoggT m) where
  fmap f (LoggT m) = LoggT $ fmap (fmap f) m

instance Monad m => Applicative (LoggT m) where
  pure x = LoggT $ return (Logged "" x)
  (LoggT mf) <*> (LoggT m) = LoggT $ liftA2 (<*>) mf m

instance Monad m => Monad (LoggT m) where
  return = pure
  (>>=) :: LoggT m a -> (a -> LoggT m b) -> LoggT m b
  m >>= k =
    LoggT $ do
      (Logged str1 a) <- runLoggT m
      (Logged str2 b) <- runLoggT $ k a
      return $ Logged (str1 `mappend` str2) b

instance Fail.MonadFail m => Fail.MonadFail (LoggT m) where
  fail str = LoggT $ Fail.fail str

write2log :: Monad m => String -> LoggT m ()
write2log str = LoggT . return $ Logged str ()

type Logg = LoggT Identity

runLogg :: Logg a -> Logged a
runLogg = runIdentity . runLoggT

instance MonadTrans LoggT where
  lift = LoggT . fmap (Logged "")

logTst :: LoggT Identity Integer
logTst = do
  x <- LoggT $ Identity $ Logged "AAA" 30
  y <- return 10
  z <- LoggT $ Identity $ Logged "BBB" 2
  return $ x + y + z

example1 = runIdentity (runLoggT logTst) -- Logged "AAABBB" 42

failTst :: [Integer] -> LoggT [] Integer
failTst xs = do
  5 <- LoggT $ fmap (Logged "") xs
  LoggT [Logged "A" ()]
  return 42

example2 = runLoggT $ failTst [5, 5] -- [Logged "A" 42,Logged "A" 42]

example3 = runLoggT $ failTst [5, 6] -- [Logged "A" 42]

example4 = runLoggT $ failTst [7, 6] -- []
