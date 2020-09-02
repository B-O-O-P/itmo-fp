{-# LANGUAGE InstanceSigs #-}

module Block2
       ( splitOn
       , joinWith
       ) where

import Block3 (NonEmpty (..))

-- Task 2

-- | Splits the list into sub-lists by item.
splitOn :: Eq a => a -> [a] -> NonEmpty [a]
splitOn separator = foldr (split separator) ([] :| [])
  where
    split :: Eq a => a -> a -> NonEmpty [a] -> NonEmpty [a]
    split sep current (x :| xs)
      | current == sep = [] :| (x : xs)
      | otherwise      = (current : x) :| xs

-- | Joins sublists into a list using a separator.
joinWith :: Eq a => a -> NonEmpty [a] -> [a]
joinWith separator = foldr (join separator) []
  where
    join :: Eq a => a -> [a] -> [a] -> [a]
    join _ current []     = current
    join sep current list = current ++ (sep : list)
    