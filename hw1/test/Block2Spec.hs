{-# OPTIONS_GHC -fno-warn-type-defaults #-}

module Block2Spec
       ( spec 
       ) where

import Data.Foldable (Foldable (..))
import Data.List (sort)
import Test.Hspec

import Block1 (Tree (..), fromList)
import Block2
import Block3 (NonEmpty (..))

spec :: Spec
spec = do
  describe "Task 1: Foldable tree" $ do
    it "foldmap test" $ do
      foldMap (:[]) (fromList [0])    `shouldBe` [0]
      foldMap (:[]) (fromList [0..3]) `shouldBe` [0..3]

    it "toList fromList returns same sorted list" $ do
      toList (fromList [0])          `shouldBe` [0]
      toList (fromList [3, 1, 2, 0]) `shouldBe` sort [3, 1, 2, 0]

  describe "Task 2: splitOn and joinWith" $ do
    it "splitOn returns splitted non-empty list" $ do
      splitOn '/' "path/to/file" `shouldBe` ("path" :| ["to", "file"])

    it "joinWith returns joined string" $ do
      joinWith '/' ("path" :| ["to", "file"]) `shouldBe` "path/to/file"
