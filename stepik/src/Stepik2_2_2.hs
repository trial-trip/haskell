{-# LANGUAGE RankNTypes    #-}
{-# LANGUAGE TypeOperators #-}

module Stepik2_2_2 where

import           Control.Applicative (Alternative (..), getZipList, liftA2,
                                      liftA3)
import qualified Data.Foldable       as F

data Tree a
  = Nil
  | Branch (Tree a) a (Tree a)
  deriving (Eq, Show)

instance Functor Tree where
  fmap f Nil            = Nil
  fmap f (Branch l x r) = Branch (fmap f l) (f x) (fmap f r)

instance Foldable Tree where
  foldr f ini Nil            = ini
  foldr f ini (Branch l x r) = f x $ foldr f (foldr f ini r) l

instance Traversable Tree where
  traverse g Nil = pure Nil
  traverse g (Branch l x r) = liftA3 Branch (traverse g l) (g x) (traverse g r)

testTree = Branch (Branch (Branch Nil 1 Nil) 2 (Branch Nil 3 Nil)) 4 (Branch Nil 5 Nil) -- [4,2,1,3,5]

treeX = Branch (Branch Nil 1 Nil) 2 (Branch (Branch Nil 3 Nil) 4 (Branch Nil 5 Nil))

treeExample = fst $ sequenceA_ $ (\x -> (show x, x)) <$> treeX

fold :: (Foldable t, Monoid m) => t m -> m
fold = foldr mappend mempty

-- сумма альтернатив. вообще, то же самое, что fold, только для альтернатив, а не моноидов
asum :: (Foldable t, Alternative f) => t (f a) -> f a
asum = foldr (<|>) empty

sequenceA_ :: (Foldable t, Applicative f) => t (f a) -> f ()
sequenceA_ = foldr (*>) (pure ())
  -- выполняет эффекты, возвращает effectful контейнер, игнорируя значение
  -- sequenceA_ (const Nothing <$> testTree) -- Nothing (первый встретившийся Nothing)
  -- sequenceA_ (Just <$> testTree) -- Just ()
  -- sequenceA_ [[1,2,3], [4,5]] -- [(),(),(),(),(),()]
  -- sequenceA_ [("AB", 1), ("CD", 2)] -- ("ABCD",())

sequenceA2list :: (Foldable t, Applicative f) => t (f a) -> f [a]
sequenceA2list = foldr (\x y -> pure (:) <*> x <*> y) (pure [])
  -- sequenceA2list [("AB", 1), ("CD", 2)] -- ("ABCD",[1,2])
  -- sequenceA2list [Right 1, Right 2]  -- Right [1, 2]
  -- sequenceA2list [Right 1, Left 2]  -- Left 2

foldMap :: (Foldable t, Monoid m) => (a -> m) -> t a -> m
foldMap f = foldr (mappend . f) mempty

traverse_ :: (Foldable t, Applicative f) => (a -> f b) -> t a -> f ()
traverse_ f = foldr ((*>) . f) (pure ())
  -- traverse_ - это как foldMap, только вместо моноида - Applicative
  -- (*>) :: f a -> f () -> f ()
  -- traverse (\x -> (show x, 2 * x)) [1, 2] -- ("12",[2,4])
  -- traverse_ (\x -> (show x, 2 * x)) [1, 2] -- ("12",())
  -- traverse (\x -> if x > 0 then Right x else Left x) [1, 2] -- Right [1,2]
  -- traverse_ (\x -> if x > 0 then Right x else Left x) [1, 2] -- Right ()
  -- traverse_ (\x -> if x > 0 then Right x else Left x) [1, -2] -- Left (-2)
  -- traverse_ (\x -> if x > 0 then Right x else Left x) [-1, -2] -- Left (-1)
  -- traverse_ (\x -> if x > 0 then Right x else Left x) [-1, 2] -- Left (-1)
  --
  -- traverse_ (\x -> [x + 10, x + 20]) [1,2,3] -- [(), (), (), (), (), (), (), ()]
  -- traverse_ f [x1, x2, x3] = f x1 *> (f x2 *> (f x3 *> pure ()))
  -- = [11, 21] *> ([12, 22] *> ([13, 23] *> [()]))
  -- = с каждым 2 эффекта (с каждым 2 эффекта (2 эффекта))

traverse2list :: (Foldable t, Applicative f) => (a -> f b) -> t a -> f [b]
traverse2list f = foldr (liftA2 (:) . f) (pure [])
  -- traverse2list f = foldr ((\x y -> pure (:) <*> x <*> y) . f) (pure [])
  -- traverse2list (\x -> [x + 10, x + 20]) [1, 2, 3] --
  -- [[11,12,13],[11,12,23],[11,22,13],[11,22,23],
  -- [21,12,13],[21,12,23],[21,22,13],[21,22,23]]

data Triple a =
  Tr a a a
  deriving (Eq, Show)

instance Foldable Triple where
  foldl f ini (Tr a b c) = foldl f ini [a, b, c]
  foldr f ini (Tr a b c) = foldr f ini [a, b, c]

instance Functor Triple where
  fmap f (Tr a b c) = Tr (f a) (f b) (f c)

instance Traversable Triple where
  traverse g (Tr a b c) = liftA3 Tr (g a) (g b) (g c)
    -- from left to right, and collect the results. For a version that ignores
    -- the results see 'Data.Foldable.traverse_'.
    --    traverse :: Applicative f => (a -> f b) -> t a -> f (t b)
    --    {-# INLINE traverse #-}  -- See Note [Inline default methods]
    --    traverse f = sequenceA . fmap f
    --

traversableExample1 = foldl (++) "!!" (Tr "ab" "cd" "efg") -- "!!abcdefg"
    -- | Map each element of a structure to an action, evaluate these actions

traversableExample2 =
  traverse
    (\x ->
       if x > 10
         then Right x
         else Left x)
    (Tr 12 14 16) -- Right (Tr 12 14 16)

data Result a
  = Ok a
  | Error String
  deriving (Eq, Show)

instance Foldable Result where
  foldl f ini (Error _) = ini
  foldl f ini (Ok x)    = f ini x
  foldr f ini (Error _) = ini
  foldr f ini (Ok x)    = f x ini

instance Functor Result where
  fmap g (Ok a)    = Ok $ g a
  fmap _ (Error s) = Error s

instance Traversable Result where
  traverse g (Ok a)    = Ok <$> g a
  traverse _ (Error s) = pure $ Error s

traversableResultExample = traverse (\x -> [x + 2, x - 2]) (Ok 5)
  -- [Ok 7,Ok 3]

traversableTreeExample =
  traverse
    (\x ->
       if odd x
         then Right x
         else Left x)
    (Branch (Branch Nil 1 Nil) 3 Nil)
  -- Right (Branch (Branch Nil 1 Nil) 3 Nil)

infixr 9 |.|

newtype (|.|) f g a =
  Cmps
    { getCmps :: f (g a)
    }
  deriving (Eq, Show)

instance (Functor f, Functor g) => Functor ((|.|) f g) where
  fmap f m = Cmps $ fmap f <$> getCmps m

instance (Foldable f, Foldable g) => Foldable ((|.|) f g) where
  foldMap fm = F.foldMap (F.foldMap fm) . getCmps

instance (Traversable f, Traversable g) => Traversable ((|.|) f g) where
  traverse f m = Cmps <$> traverse (traverse f) (getCmps m)

traversableComposeExample = sequenceA (Cmps [Just (Right 2), Nothing])
  -- Right (Cmps {getCmps = [Just 2,Nothing]})

currentExample = traversableResultExample
