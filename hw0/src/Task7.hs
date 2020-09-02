{-# LANGUAGE ScopedTypeVariables #-}

module Task7
  ( first
  , second
  , third
  ) where

import Data.Either(lefts, rights)

-- | This function checks if head of list ["Dorian Grey"] is null.
-- null . head $ map (uncurry id) [((++) "Dorian ", " Grey")]
first :: Bool
first = mFunc `dollar` mArgument
 where
  dollar :: (a -> b) -> a -> b
  dollar = ($)

  mFunc :: [String] -> Bool
  mFunc = mNull `dot` mHead
    where
      mNull :: [a] -> Bool
      mNull = null

      dot :: (b -> c) -> (a -> b) -> a -> c
      dot = (.)

      mHead :: [a] -> a
      mHead = head

  mArgument :: [String]
  mArgument = mMap mMapFunc mList
    where
      mMap :: (a -> b) -> [a] -> [b]
      mMap = map

      mMapFunc :: (String -> String, String) -> String
      mMapFunc = mUncurry mId
        where
          mUncurry :: (a -> b -> c) -> (a, b) -> c
          mUncurry = uncurry

          mId :: a -> a
          mId = id


      mList :: [(String -> String, String)]
      mList = [(mConcat mDorian, mGrey)]
        where
          mConcat = (++)

          mDorian :: String
          mDorian = "Dorian "

          mGrey :: String
          mGrey = " Grey"

-- | Construct pair of 3 and 64.
-- (\x -> zip (lefts x) (rights x)) [Left (1 + 2), Right (2 ^ 6)]
second :: [(Integer, Integer)]
second = mLambda mList
  where
    mLambda :: forall a b . [Either a b] -> [(a, b)]
    mLambda = (\x ->
      mZip
      ((lefts :: [Either a b] -> [a]) (x :: [Either a b]))
      ((rights :: [Either a b] -> [b]) (x :: [Either a b])))
        where
          mZip :: [a] -> [b] -> [(a, b)]
          mZip = zip

    mList :: (Num a, Num b) => [Either a b]
    mList = [mFirst, mSecond]
      where
        mFirst :: Num a => Either a b
        mFirst = mLeft (mFirstNum)
          where
            mLeft :: a -> Either a b
            mLeft = Left

            mFirstNum :: Num a => a
            mFirstNum = mOne `mPlus` mThree
              where
                mOne :: Num a => a
                mOne = 1

                mPlus :: Num a => a -> a -> a
                mPlus = (+)

                mThree :: Num a => a
                mThree = 3

        mSecond :: Num b => Either a b
        mSecond = mRight (mSecondNum)
          where
            mRight :: b -> Either a b
            mRight = Right

            mSecondNum :: Num a => a
            mSecondNum = mTwo `mPow` mSix
             where
              mTwo :: Num a => a
              mTwo = 2

              mPow :: (Integral b, Num a) => a -> b -> a
              mPow = (^)

              mSix :: Num a => a
              mSix = 6

-- | Checks if a number is divisible by 2 or not divisible by 4.
-- let impl = \x y -> not x || y in
--     let isMod2 = \x -> x `mod` 2 == 0 in
--     let isMod4 = \x -> x `mod` 4 == 0 in
--     \x -> (isMod4 x) `impl` (isMod2 x)
third :: forall a . Integral a => a -> Bool
third = let impl :: Bool -> Bool -> Bool
            impl = \x y ->
                   (((||) :: Bool -> Bool -> Bool) 
                   (((not :: Bool -> Bool) (x :: Bool)) :: Bool)  
                   (y :: Bool) :: Bool) in

            let isMod2 :: Integral a => a -> Bool
                isMod2 = \x ->
                  (((==) :: Eq a => a -> a -> Bool ) 
                  (((mod :: Integral a => a -> a -> a) (x :: Integral a => a) 
                  (2 :: Num a => a)) :: Integral a => a)
                  (0 :: Num a => a)) :: Bool in

            let isMod4 :: Integral a => a -> Bool
                isMod4 = \x ->
                  (((==) :: Eq a => a -> a -> Bool ) 
                  (((mod :: Integral a => a -> a -> a) (x :: Integral a => a) 
                  (4 :: Num a => a)) :: Integral a => a) 
                  (0 :: Num a => a)) :: Bool in

            \x -> 
              (isMod4 (x :: Integral a => a) :: Bool) `impl` 
              (isMod2 (x :: Integral a => a) :: Bool) :: Bool
