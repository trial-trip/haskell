{-# LANGUAGE RankNTypes    #-}
{-# LANGUAGE TypeOperators #-}

module Stepik2_1_x where

import           Control.Applicative (Alternative (..), ZipList (..),
                                      getZipList, liftA2, (<**>))
import           Data.Char           (isDigit)
import           Data.Function       ((&))

--import           Text.Parsec
--import           Text.Parsec.String  (Parser)
newtype Arr2 e1 e2 a =
  Arr2
    { getArr2 :: e1 -> e2 -> a
    }

instance Functor (Arr2 e1 e2) where
  fmap f m = Arr2 (\e1 e2 -> f $ getArr2 m e1 e2)

instance Applicative (Arr2 e1 e2) where
  pure x = Arr2 $ \e1 e2 -> x
  f <*> m = Arr2 $ \e1 e2 -> getArr2 f e1 e2 $ getArr2 m e1 e2

newtype Arr3 e1 e2 e3 a =
  Arr3
    { getArr3 :: e1 -> e2 -> e3 -> a
    }

instance Functor (Arr3 e1 e2 e3) where
  fmap f m = Arr3 (\e1 e2 e3 -> f $ getArr3 m e1 e2 e3)

instance Applicative (Arr3 e1 e2 e3) where
  pure x = Arr3 $ \e1 e2 e3 -> x
  z <*> x = Arr3 $ \e1 e2 e3 -> getArr3 z e1 e2 e3 $ getArr3 x e1 e2 e3

data Triple a =
  Tr a a a
  deriving (Eq, Show)

instance Functor Triple where
  fmap f (Tr e1 e2 e3) = Tr (f e1) (f e2) (f e3)

instance Applicative Triple where
  pure x = Tr x x x
  (Tr f1 f2 f3) <*> (Tr e1 e2 e3) = Tr (f1 e1) (f2 e2) (f3 e3)

(>$<) :: (a -> b) -> [a] -> [b]
f >$< xs = getZipList $ f <$> ZipList xs

(>*<) :: [a -> b] -> [a] -> [b]
xs >*< ys = getZipList $ ZipList xs <*> ZipList ys

--getList :: Parsec String u [String]
--getList = many1 digit `sepBy1` char ';'
--
--ignoreBraces :: Parsec String u a -> Parsec String u b -> Parsec String u c -> Parsec String u c
--ignoreBraces a b c = liftA2 (flip const) a (liftA2 const c b) -- работает
--
--test = ignoreBraces (string "[[") (string "]]") (many1 letter)
newtype Prs a =
  Prs
    { runPrs :: String -> Maybe (a, String)
    }

instance Functor Prs where
  fmap f p = Prs $ \s -> (\(a, str) -> (f a, str)) <$> runPrs p s

instance Applicative Prs where
  pure x = Prs (\str -> Just (x, str))
  p1 <*> p2 =
    Prs $ \str ->
      case runPrs p1 str of
        Just (f, str2) -> runPrs (f <$> p2) str2
        Nothing        -> Nothing

instance Alternative Prs where
  empty = Prs $ const Nothing
  p1 <|> p2 =
    Prs $ \str ->
      case runPrs p1 str of
        Nothing -> runPrs p2 str
        x       -> x

satisfyM :: (Char -> Bool) -> Prs Char
satisfyM predicate = Prs f
  where
    f "" = Nothing
    f (x:xs)
      | predicate x = Just (x, xs)
      | otherwise = Nothing

charM :: Char -> Prs Char
charM c = satisfyM (== c)

manyX :: Prs a -> Prs [a]
manyX p = (:) <$> p <*> manyX p <|> pure []

many1 :: Prs a -> Prs [a]
many1 p =
  Prs $ \str ->
    case runPrs p str of
      Just (x, str2) -> (\(Just (xs, b)) -> Just (x : xs, b)) $ runPrs (manyX p) str2
      Nothing -> Nothing

nat :: Prs Int
nat = read <$> many1 (satisfyM isDigit)

mult :: Prs Int
mult = (*) <$> nat <* charM '*' <*> nat

anyChr = Prs f
  where
    f ""     = Nothing
    f (x:xs) = Just (x, xs)

newtype PrsE a =
  PrsE
    { runPrsE :: String -> Either String (a, String)
    }

instance Functor PrsE where
  fmap f p = PrsE $ \s -> (\(a, str) -> (f a, str)) <$> runPrsE p s

instance Applicative PrsE where
  pure x = PrsE (\str -> Right (x, str))
  p1 <*> p2 =
    PrsE $ \str ->
      case runPrsE p1 str of
        Right (f, str2) -> runPrsE (f <$> p2) str2
        Left x          -> Left x

satisfyE :: (Char -> Bool) -> PrsE Char
satisfyE predicate = PrsE f
  where
    f "" = Left "unexpected end of input"
    f (x:xs)
      | predicate x = Right (x, xs)
      | otherwise = Left $ "unexpected " ++ [x]

charE :: Char -> PrsE Char
charE c = satisfyE (== c)

infixr 9 |.|

newtype (|.|) f g a =
  Cmps
    { getCmps :: f (g a)
    }
  deriving (Eq, Show)

type A = ((,) Integer |.| (,) Char) Bool

type B t = ((,,) Bool (t -> t) |.| Either String) Int

type C = (|.|) ((->) Bool) ((->) Integer) Integer

a :: A
a = Cmps (42, ('c', True))

b :: B t
b = Cmps (True, id, Right 42)

c :: C
c = Cmps $ const id

newtype Cmps3 f g h a =
  Cmps3
    { getCmps3 :: f (g (h a))
    }
  deriving (Eq, Show)

instance (Functor f, Functor g, Functor h) => Functor (Cmps3 f g h) where
  fmap f m = Cmps3 $ fmap (fmap (fmap f)) $ getCmps3 m

unCmps3 :: Functor f => (f |.| g |.| h) a -> f (g (h a))
unCmps3 x = getCmps <$> getCmps x

unCmps4 :: (Functor f2, Functor f1) => (f2 |.| f1 |.| g |.| h) a -> f2 (f1 (g (h a)))
unCmps4 x = (fmap getCmps . getCmps) <$> getCmps x
