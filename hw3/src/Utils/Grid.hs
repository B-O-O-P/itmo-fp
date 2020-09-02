{-# LANGUAGE  InstanceSigs #-}
{-# LANGUAGE  DeriveFunctor #-}

module Utils.Grid
    ( Grid(..)
    , PrintableGrid(..)
    , neighbours
    , gridRead
    , gridWrite
    ) where

import Control.Comonad
import Data.List (intercalate)

import Utils.ListZipper

-- | Data type, which represents infinite grid with one focused element.
newtype Grid a = Grid { unGrid :: ListZipper (ListZipper a) }
  deriving Functor

-- | Change row up of focused position.
up :: Grid a -> Grid a
up   (Grid g) = Grid (listLeft g)

-- | Change row down of focused position.
down :: Grid a -> Grid a
down (Grid g) = Grid (listRight g)

-- | Change column left of focused position.
left :: Grid a -> Grid a
left  (Grid g) = Grid (fmap listLeft g)

-- | Change column right of focused position.
right :: Grid a -> Grid a
right (Grid g) = Grid (fmap listRight g)

-- | Get focused element value.
gridRead :: Grid a -> a
gridRead (Grid g) = extract $ extract g

-- | Change focused element value.
gridWrite :: a -> Grid a -> Grid a
gridWrite x (Grid g) = Grid $ listWrite newLine g
  where
    oldLine = extract g
    newLine = listWrite x oldLine

-- | Get zipper of all columns.
horizontal :: Grid a -> ListZipper (Grid a)
horizontal = genericMove left right

-- | Get zipper of all rows.
vertical :: Grid a -> ListZipper (Grid a)
vertical = genericMove up down

-- | Get all neighbours of element.
neighbours :: [Grid a -> Grid a]
neighbours = horizontals ++ verticals
  where
    horizontals = [left, right]
    verticals   = [up, down]

instance Comonad Grid where
    extract :: Grid a -> a
    extract = gridRead

    duplicate :: Grid a -> Grid (Grid a)
    duplicate = Grid . fmap horizontal . vertical

-- | Data type for printing Grid. Consists of two parameters - Grid and radius of which Grid will be printed.
data PrintableGrid a = PrintableGrid
  { grid   :: Grid a
  , radius :: Int
  }

instance Show a => Show (PrintableGrid a) where
  show :: PrintableGrid a -> String
  show (PrintableGrid (Grid g) rad) =
    intercalate "\n" (map (\lz -> concatMap show (toList lz rad)) (toList g rad))
