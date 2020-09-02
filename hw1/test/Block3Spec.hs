{-# OPTIONS_GHC -fno-warn-type-defaults #-}

module Block3Spec
       ( spec 
       ) where

import Data.Monoid (Sum (..))
import Test.Hspec

import Block3

spec :: Spec
spec = do
  describe "Task 1: maybeConcat and eitherConcat" $ do
    it "maybeConcat concatenates only Just" $ do
      maybeConcat [Just [1,2,3], Nothing,       Just [4,5]] `shouldBe` [1..5]
      maybeConcat [Nothing,      Just[3, 2, 1], Nothing]    `shouldBe` [3, 2, 1]

    it "eitherConcat returns calculated union" $ do
      eitherConcat [Left (Sum 3), Right [1,2,3], Left (Sum 5), Right [4,5]]
        `shouldBe` (Sum {getSum = 8}, [1,2,3,4,5])
      eitherConcat [Left (Sum 5), Right [3, 2], Right [1], Left (Sum 5)]
        `shouldBe` (Sum {getSum = 10}, [3, 2, 1])

  describe "Task 2: NonEmpty and ThisOrThat Semigroups" $ do
    it "NonEmpty <> concatenates one by one" $ do
      (0 :| [1..4]) <> (5 :| [6..10])             `shouldBe` (0 :| [1..10])
      (3 :| [1, 2]) <> (4 :| [5])                 `shouldBe` (3 :| [1, 2, 4, 5])
      (1 :| [2])    <> (3 :| [4,5]) <> (6 :| [7]) `shouldBe` (1 :| [2..7])
