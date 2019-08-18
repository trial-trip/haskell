module Stepik2_3_4 where

import qualified Control.Monad.Fail         as Fail
import           Control.Monad.Trans        (lift)
import           Control.Monad.Trans.Class
import           Control.Monad.Trans.Reader
import           Control.Monad.Trans.Writer
import           Data.Char                  (toUpper)
import           Data.Functor.Identity
import           Data.List                  (partition)

newtype Arr2T e1 e2 m a =
  Arr2T
    { getArr2T :: e1 -> e2 -> m a
    }

instance Functor m => Functor (Arr2T e1 e2 m) where
  fmap f m = Arr2T $ \e1 e2 -> f <$> getArr2T m e1 e2

instance Applicative m => Applicative (Arr2T e1 e2 m) where
  pure x = Arr2T $ \e1 e2 -> pure x
  mf <*> m = Arr2T $ \e1 e2 -> getArr2T mf e1 e2 <*> getArr2T m e1 e2

instance Monad m => Monad (Arr2T e1 e2 m) where
  m >>= k =
    Arr2T $ \e1 e2 -> do
      v <- getArr2T m e1 e2
      getArr2T (k v) e1 e2

instance MonadTrans (Arr2T e1 e2) where
  lift m = Arr2T $ \e1 e2 -> m

asks2 :: Monad m => (e1 -> e2 -> a) -> Arr2T e1 e2 m a
asks2 f2 = Arr2T $ \e1 e2 -> return $ f2 e1 e2

newtype Arr3T e1 e2 e3 m a =
  Arr3T
    { getArr3T :: e1 -> e2 -> e3 -> m a
    }

instance Functor m => Functor (Arr3T e1 e2 e3 m) where
  fmap f m = Arr3T $ \e1 e2 e3 -> f <$> getArr3T m e1 e2 e3

instance Applicative m => Applicative (Arr3T e1 e2 e3 m) where
  pure x = Arr3T $ \e1 e2 e3 -> pure x
  mf <*> m = Arr3T $ \e1 e2 e3 -> getArr3T mf e1 e2 e3 <*> getArr3T m e1 e2 e3

instance Monad m => Monad (Arr3T e1 e2 e3 m) where
  m >>= k =
    Arr3T $ \e1 e2 e3 -> do
      v <- getArr3T m e1 e2 e3
      getArr3T (k v) e1 e2 e3

instance Fail.MonadFail m => Fail.MonadFail (Arr3T e1 e2 e3 m) where
  fail str = Arr3T $ \_ _ _ -> Fail.fail str

arr2 :: Monad m => (e1 -> e2 -> a) -> Arr2T e1 e2 m a
arr2 f2 = Arr2T $ \e1 e2 -> return $ f2 e1 e2

arr3 :: Monad m => (e1 -> e2 -> e3 -> a) -> Arr3T e1 e2 e3 m a
arr3 f3 = Arr3T $ \e1 e2 e3 -> return $ f3 e1 e2 e3

example1 = (getArr2T $ arr2 (+)) 33 9 :: [Integer] -- [42]

example2 = (getArr3T $ arr3 foldr) (*) 1 [1 .. 5] :: Either String Integer -- Right 120

example3 = runIdentity $ (getArr2T $ arr2 (+)) 33 9 -- 42

a2l = Arr2T $ \e1 e2 -> [e1, e2, e1 + e2]

functor2 = (getArr2T $ succ <$> a2l) 10 100 --[11,101,111]

a3e = Arr3T $ \e1 e2 e3 -> Right (e1 + e2 + e3)

functor3 :: Either String Double
functor3 = (getArr3T $ sqrt <$> a3e) 2 3 4 -- Right 3.0

a2l' = Arr2T $ \e1 e2 -> [e1, e2]

a2fl' = Arr2T $ \e1 e2 -> [(e1 * e2 +), const 7]

applicative2 = getArr2T (a2fl' <*> a2l') 2 10 -- [22,30,7,7]

a3l' = Arr3T $ \e1 e2 e3 -> [e1, e2]

a3fl' = Arr3T $ \e1 e2 e3 -> [(e2 +), (e3 +)]

applicative3 = getArr3T (a3fl' <*> a3l') 3 5 7 -- [8,10,10,12]

a2l'' = Arr2T $ \e1 e2 -> [e1, e2]

monad2 =
  getArr2T
    (do x <- a2l''
        y <- a2l''
        return (x + y))
    3
    5
  -- [6,8,8,10]

a3m'' = Arr3T $ \e1 e2 e3 -> Just (e1 + e2 + e3)

monad3 =
  getArr3T
    (do x <- a3m''
        y <- a3m''
        return (x * y))
    2
    3
    4
  -- Just 81

monadFailExample =
  getArr3T
    (do 10 <- a3m''
        y <- a3m''
        return y)
    2
    3
    4 -- Nothing

a2l''' = Arr2T $ \e1 e2 -> [e1, e2]

liftExample =
  getArr2T
    (do x <- a2l'''
        y <- lift [10, 20, 30]
        return (x + y))
    3
    4
    --- [13,23,33,14,24,34]

asks2Example :: Identity (Char, Char, (Char, Char))
asks2Example =
  getArr2T
    (do x <- asks2 const
        y <- asks2 (flip const)
        z <- asks2 (,)
        return (x, y, z))
    'A'
    'B'
  -- ('A','B',('A','B'))
