module Task4
  ( iterateElement
  , fibonacci
  , factorial
  , mapFix
  ) where

import Data.Function (fix)

-- fix :: (a -> a) -> a
-- fix f = f (fix f)

-- | Function to iterate through list.
iterateElement :: a -> [a]
iterateElement a = fix (a :)

-- | Function to count Fibonacci numbers.
fibonacci :: Integer -> Integer
fibonacci = fix fibRecursion
  where
    fibRecursion :: (Integer -> Integer) -> Integer -> Integer
    fibRecursion f x
      | x < 0                = 0
      | (x >= 0) && (x <= 1) = 1
      | otherwise            = f (x - 1) + f (x - 2)

-- | Function to count factorial.
factorial :: Integer -> Integer
factorial = fix factRecursion
  where
    factRecursion :: (Integer -> Integer) -> Integer -> Integer
    factRecursion f x
      | x <= 1    = 1
      | otherwise = x * f (x - 1)

-- | Recursive map function implementation.
mapFix :: (a -> b) -> [a] -> [b]
mapFix = fix mapRec
  where
    mapRec :: ((a -> b) -> [a] -> [b]) -> (a -> b) -> [a] -> [b]
    mapRec _ _ []  = []
    mapRec rec f x = f (head x) : rec f (tail x)
