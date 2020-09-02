{-# LANGUAGE InstanceSigs #-}

module Block4
       ( stringSum
       , Tree (..)
       , NonEmpty (..)
       ) where

import Text.Read (readMaybe)

-- Task 1

-- | Sums the numbers in a string.
stringSum :: String -> Maybe Int
stringSum = fmap sum . traverse readMaybe . words

-- Task 2

-- | Binary tree data type.
data Tree a
  = Branch (Tree a) (Tree a)
  | Leaf a

instance Functor Tree where
  fmap :: (a -> b) -> Tree a -> Tree b
  fmap f (Leaf x)            = Leaf (f x)
  fmap f (Branch left right) = Branch (fmap f left) (fmap f right)

instance Applicative Tree where
  pure :: a -> Tree a
  pure = Leaf
  
  (<*>) :: Tree (a -> b) -> Tree a -> Tree b
  (Leaf f)            <*> tree = fmap f tree
  (Branch left right) <*> tree = Branch (left <*> tree) (right <*> tree)

instance Foldable Tree where
  foldMap :: Monoid m => (a -> m) -> Tree a -> m
  foldMap f (Leaf x)            = f x
  foldMap f (Branch left right) = foldMap f left <> foldMap f right

instance Traversable Tree where
  traverse :: Applicative f => (a -> f b) -> Tree a -> f (Tree b)
  traverse f (Leaf x)            = Leaf <$> f x
  traverse f (Branch left right) = Branch <$> traverse f left <*> traverse f right

-- Task 3

-- | Data type of non-empty list.
data NonEmpty a =
  a :| [a]
  deriving (Show)

instance Functor NonEmpty where
  fmap :: (a -> b) -> NonEmpty a -> NonEmpty b
  fmap f (x :| list) = f x :| (f <$> list)

instance Applicative NonEmpty where
  pure :: a -> NonEmpty a
  pure x = x :| []
  
  (<*>) :: NonEmpty (a -> b) -> NonEmpty a -> NonEmpty b
  (f :| fs) <*> (x :| xs) = f x :| ((f <$> xs) ++ (fs <*> (x:xs)))

instance Monad NonEmpty where
  return :: a -> NonEmpty a
  return x = x :| []

  (>>=) :: NonEmpty a -> (a -> NonEmpty b) -> NonEmpty b
  x :| xs >>= f = foldl merge (f x) (map f xs)
    where
      merge :: NonEmpty a -> NonEmpty a -> NonEmpty a
      merge (x :| xs) (b :| bs) = x :| (xs ++ b : bs)

instance Foldable NonEmpty where
  foldMap :: Monoid m => (a -> m) -> NonEmpty a -> m
  foldMap f (x :| xs) = f x <> foldMap f xs

instance Traversable NonEmpty where
  traverse :: Applicative f => (a -> f b) -> NonEmpty a -> f (NonEmpty b)
  traverse f (x :| xs) = fmap (:|) (f x) <*> traverse f xs
