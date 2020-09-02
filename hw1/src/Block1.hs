{-# LANGUAGE InstanceSigs #-}

module Block1
       ( WeekDay (..)
       , nextDay
       , afterDays
       , isWeekend
       , daysToParty
       
        , Nat (..)
        , fromNat
        , isEven
        , (//)
        , (%)
        
        , Tree (..)
        , isEmpty
        , size
        , find
        , insert
        , fromList
       ) where

import Block3 (NonEmpty (..))

-- Task 1

-- | ADT for days of the week.
data WeekDay
  = Monday
  | Tuesday
  | Wednesday
  | Thursday
  | Friday
  | Saturday
  | Sunday
  deriving (Show)

-- | Returns next day of the week.
-- Looped from Monday to Sunday.
nextDay :: WeekDay -> WeekDay
nextDay Monday    = Tuesday
nextDay Tuesday   = Wednesday
nextDay Wednesday = Thursday
nextDay Thursday  = Friday
nextDay Friday    = Saturday
nextDay Saturday  = Sunday
nextDay Sunday    = Monday

-- | Returns the day of the week that comes after the specified number of days passed.
afterDays :: WeekDay -> Int -> WeekDay
afterDays day 0 = day
afterDays day n = nextDay (afterDays day (n - 1))

-- | Checks if the day of the week is a weekend.
isWeekend :: WeekDay -> Bool
isWeekend Saturday = True
isWeekend Sunday   = True
isWeekend _        = False

-- | Counts the number of days remaining until Friday.
daysToParty :: WeekDay -> Int
daysToParty Friday = 0
daysToParty day    = daysToParty (nextDay day) + 1

-- Only for tests and debug

instance Eq WeekDay where
  (==) :: WeekDay -> WeekDay -> Bool
  a == b = show a == show b 

-- Task 2

-- | Data type for natural numbers.
data Nat
  = Z
  | S Nat
  deriving (Show)

instance Num Nat where
  (+) :: Nat -> Nat -> Nat
  Z + b     = b
  (S a) + b = S (a + b)
  
  (*) :: Nat -> Nat -> Nat
  Z * _     = Z
  (S a) * b = (a * b) + b
  
  (-) :: Nat -> Nat -> Nat
  Z - _         = Z
  a - Z         = a
  (S a) - (S b) = a - b

  negate :: Nat -> Nat
  negate Z = Z
  negate _ = error "Nat cannot be negated"

  abs :: Nat -> Nat
  abs = id

  signum :: Nat -> Nat
  signum Z = 0
  signum _ = 1

  fromInteger :: Integer -> Nat
  fromInteger x
    | x < 0 = error "Argument must be at least 0"
    | x == 0 = Z
    | otherwise = S (fromInteger (x - 1))

-- | Converts natural number to integer number.
fromNat :: Nat -> Integer
fromNat Z     = 0
fromNat (S a) = 1 + fromNat a

instance Eq Nat where
  (==) :: Nat -> Nat -> Bool
  a == b = compare a b == EQ

instance Ord Nat where
  compare :: Nat -> Nat -> Ordering
  compare Z Z         = EQ
  compare Z _         = LT
  compare _ Z         = GT
  compare (S a) (S b) = compare a b

-- Hard version

-- | Checks a natural number for parity.
isEven :: Nat -> Bool
isEven Z         = True
isEven (S Z)     = False
isEven (S (S a)) = isEven a

-- | Integer division of natural numbers.
(//) :: Nat -> Nat -> Nat
(//) _ Z = error "Second argument must be natural number and more than zero"
(//) a b
  | a < b     = Z
  | otherwise = S ((a - b) // b)

-- | The remainder of dividing a natural number by another.
(%) :: Nat -> Nat -> Nat
(%) a b
  | a < b     = a
  | otherwise = a - ((a // b) * b)
  
-- Task 3

-- | Binary tree data type.
data Tree a
  = Leaf
  | Node (NonEmpty a)
         (Tree a)
         (Tree a)
  deriving (Show)

-- | Checks a tree for emptiness.
isEmpty :: Tree a -> Bool
isEmpty Leaf = True
isEmpty _    = False

-- | Counting the size of a tree (i.e. the number of elements in it).
size :: Tree a -> Int
size Leaf                    = 0
size (Node value left right) = length value + size left + size right

-- | Search for the specified item in the tree.
find :: Ord a => Tree a -> a -> Maybe (Tree a)
find Leaf _ = Nothing
find (Node (x :| xs) left right) value
  | x == value = Just (Node (x :| xs) left right)
  | x > value = find left value
  | otherwise = find right value

-- | Insert a new item into the binary search tree.
insert :: Ord a => Tree a -> a -> Tree a
insert Leaf value = Node (value :| []) Leaf Leaf
insert (Node (x :| xs) left right) value
  | x == value = Node (value :| (x : xs)) left right
  | x > value = Node (x :| xs) (insert left value) right
  | otherwise = Node (x :| xs) left (insert right value)

-- | Creates a tree from a list of elements.
fromList :: Ord a => [a] -> Tree a
fromList []       = Leaf
fromList (x : xs) = insert (fromList xs) x

-- Task 1 from Block 2

instance Foldable Tree where
  foldMap :: Monoid m => (a -> m) -> Tree a -> m
  foldMap _ Leaf                = mempty
  foldMap f (Node x left right) = foldMap f left <> foldMap f x <> foldMap f right

  foldr :: (a -> b -> b) -> b -> Tree a -> b
  foldr _ acc Leaf                = acc
  foldr f acc (Node x left right) = foldr f (foldr f (foldr f acc right) x) left   