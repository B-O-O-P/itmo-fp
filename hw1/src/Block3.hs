{-# LANGUAGE InstanceSigs #-}

module Block3
  ( NonEmpty (..)
  , ThisOrThat (..)
  , eitherConcat
  , maybeConcat
  ) where

-- Task 1

-- | Returns the concatenation of all internal lists in list.
maybeConcat :: [Maybe [a]] -> [a]
maybeConcat = foldr f []
  where
    f :: Maybe [a] -> [a] -> [a]
    f Nothing    acc = acc
    f (Just cur) acc = cur ++ acc

-- | Returns a pair from the results of monoidal union of separately elements inside Left 
-- and separately elements inside Right.
eitherConcat :: (Monoid a, Monoid b) => [Either a b] -> (a, b)
eitherConcat = foldr append (mempty, mempty)
  where
    append :: (Monoid a, Monoid b) => Either a b -> (a, b) -> (a, b)
    append (Left left) (a, b)   = (left <> a, b)
    append (Right right) (a, b) = (a, right <> b)

-- Task 2

-- | Data type of non-empty list.
data NonEmpty a =
  a :| [a]
  deriving (Show)

instance Foldable NonEmpty where
  foldMap :: Monoid m => (a -> m) -> NonEmpty a -> m
  foldMap f (x :| xs) = f x <> foldMap f xs

  foldr :: (a -> b -> b) -> b -> NonEmpty a -> b
  foldr f z (x :| xs) = f x (foldr f z xs)

instance Semigroup (NonEmpty a) where
  (<>) :: NonEmpty a -> NonEmpty a -> NonEmpty a
  (x :| xs) <> (y :| ys) = x :| (xs ++ (y : ys))

data ThisOrThat a b
  = This a
  | That b
  | Both a b
  deriving (Show)

instance (Semigroup a, Semigroup b) => Semigroup (ThisOrThat a b) where
  (<>) :: ThisOrThat a b -> ThisOrThat a b -> ThisOrThat a b
  This a <> This b         = This (a <> b)
  This a <> That b         = Both a b
  This a <> Both b1 b2     = Both (a <> b1) b2
  That a <> This b         = Both b a
  That a <> That b         = That (a <> b)
  That a <> Both b1 b2     = Both b1 (a <> b2)
  Both a1 a2 <> This b     = Both (a1 <> b) a2
  Both a1 a2 <> That b     = Both a1 (a2 <> b)
  Both a1 a2 <> Both b1 b2 = Both (a1 <> b1) (a2 <> b2)

-- Only for tests and debug

instance (Show a) => Eq (NonEmpty a) where
  (==) :: NonEmpty a -> NonEmpty a -> Bool
  a == b = show a == show b
 
instance (Show a, Show b) => Eq (ThisOrThat a b) where 
  (==) :: ThisOrThat a b -> ThisOrThat a b -> Bool
  a == b = show a == show b