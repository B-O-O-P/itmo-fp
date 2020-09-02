{-# LANGUAGE  InstanceSigs #-}

module Utils.ListZipper
    ( ListZipper(..)
    , listLeft
    , listRight
    , listWrite
    , toList
    , genericMove
    ) where

import Control.Comonad

-- | Infinite list with focus on one element.
data ListZipper a = LZ [a] a [a]

-- | Change focus to the element on the left. Unsafe.
listLeft :: ListZipper a -> ListZipper a
listLeft (LZ (a:as) x bs) = LZ as a (x:bs)
listLeft _ = error "out of bounds from left"

-- | Change focus to the element on the right. Unsafe.
listRight :: ListZipper a -> ListZipper a
listRight (LZ as x (b:bs)) = LZ (x:as) b bs
listRight _ = error "out of bounds from right"

-- | Change focused element value.
listWrite :: a -> ListZipper a -> ListZipper a
listWrite x (LZ ls _ rs) = LZ ls x rs

-- | Convert zipper to list.
toList :: ListZipper a -> Int -> [a]
toList (LZ ls x rs) n = reverse (take n ls) ++ [x] ++ take n rs

instance Functor ListZipper where
    fmap f (LZ ls x rs) = LZ (map f ls) (f x) (map f rs)

iterateTail :: (a -> a) -> a -> [a]
iterateTail f = tail . iterate f

-- | Gengerate infinite list by two given function in two directions.
genericMove :: (a -> a) -> (a -> a) -> a -> ListZipper a
genericMove f g e = LZ (iterateTail f e) e (iterateTail g e)

instance Comonad ListZipper where
  extract :: ListZipper a -> a
  extract (LZ _ x _) = x

  duplicate :: ListZipper a -> ListZipper (ListZipper a)
  duplicate = genericMove listLeft listRight
