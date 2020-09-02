module Task1Spec
    ( spec
    ) where

import Test.Hspec (Spec, describe, it, shouldBe)

import Task1 (Point(..), doubleArea, doubleAreaNaive, perimeter, perimeterNaive)

spec :: Spec
spec = do
  describe "doubleArea" $ do
    it "calculates double area of a polygon" $ do
      doubleArea [Point 2 1, Point 4 5, Point 7 8] `shouldBe` 6

    it "calculates double area of a polygon" $ do
      doubleArea [Point 0 0, Point 0 1, Point 1 1, Point 1 0] `shouldBe` 2

  describe "doubleAreaNaive" $ do
    it "calculates double area of a polygon" $ do
      doubleAreaNaive [Point 2 1, Point 4 5, Point 7 8] `shouldBe` 6

    it "calculates double area of a polygon" $ do
      doubleAreaNaive [Point 0 0, Point 0 1, Point 1 1, Point 1 0] `shouldBe` 2

  describe "perimeter" $ do
    it "calculates perimeter of a polygon" $ do
      perimeter [Point 0 0, Point 0 1, Point 1 1, Point 1 0] `shouldBe` 4.0

  describe "perimeterNaive" $ do
    it "calculates perimeter of a polygon" $ do
      perimeterNaive [Point 0 0, Point 0 1, Point 1 1, Point 1 0] `shouldBe` 4.0
