module Utils.Task1BenchUtils
  ( generatePoint
  , generatePointList
  ) where

import Test.QuickCheck (Gen, choose, vectorOf)

import Task1 (Point (..))

generatePoint :: Gen Point
generatePoint = do
  x' <- choose (-10000000, 10000000)
  y' <- choose (-10000000, 10000000)
  return (Point x' y')

generatePointList :: Int -> Gen [Point]
generatePointList size = do
  let generatedPoint = generatePoint
  list <- vectorOf size generatedPoint
  return list
