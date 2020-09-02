{-# LANGUAGE BangPatterns #-}

module Task1
    ( Point(..)
    , plus
    , minus
    , scalarProduct
    , crossProduct
    , perimeter
    , doubleArea
    , perimeterNaive
    , doubleAreaNaive
    ) where

-- | Data type of Point, which represents point in 2D with two 'Int' coordinates.
data Point = Point
  { x :: Int
  , y :: Int
  }

-- For testing purposes only.
instance Show Point where
  show (Point x' y') = show (x', y')

-- | Implementation of the addition operator for two points.
plus :: Point -> Point -> Point
(Point x1 y1) `plus` (Point x2 y2) = Point (x1 + x2) (y1 + y2)

-- | Implementation of the difference operator for two points.
minus :: Point -> Point -> Point
(Point x1 y1) `minus` (Point x2 y2) = Point (x1 - x2) (y1 - y2)

-- | Implementation of the scalar product of two points.
scalarProduct :: Point -> Point -> Int
(Point x1 y1) `scalarProduct` (Point x2 y2) = x1 * x2 + y1 * y2

-- | Implementation of the pseudo-scalar product of two points.
crossProduct  :: Point -> Point -> Int
(Point x1 y1) `crossProduct` (Point x2 y2) = x1 * y2 - x2 * y1

-- | Calculate distance betweeen two points in 2D plane.
dist :: Point -> Point -> Double
dist (Point x1 y1) (Point x2 y2) = (sqrt . fromIntegral) (dx * dx + dy * dy)
  where
    dx = x2 - x1
    dy = y2 - y1

-- | Calculate perimeter of given polygon, which is represented by list of its points.
--  Uses 'BangPatterns'.
perimeter :: [Point] -> Double
perimeter []               = 0
perimeter coords@(point:_) = acc coords 0
  where
    acc :: [Point] -> Double -> Double
    acc []                                   _    = 0
    acc [otherPoint]                         !res = dist otherPoint point + res
    acc (firstPoint:coords'@(secondPoint:_)) !res = acc coords' (dist firstPoint secondPoint + res)

-- | Calculate doubled area of given polygon, which is represented by list of its points.
-- Uses 'BangPatterns' and Gauss's formula.
doubleArea :: [Point] -> Int
doubleArea []      = 0
doubleArea coords@(point:_) = abs $ acc coords 0
  where
    acc :: [Point] -> Int -> Int
    acc []                                   _    = 0
    acc [otherPoint]                         !res = crossProduct otherPoint point + res
    acc (firstPoint:coords'@(secondPoint:_)) !res = acc coords' (crossProduct firstPoint secondPoint + res)

-- | Calculate perimeter of given polygon, which is represented by list of its points.
-- Quite slow. For testing purposes only.
perimeterNaive :: [Point] -> Double
perimeterNaive []               = 0
perimeterNaive coords@(point:_) = acc coords 0
  where
    acc :: [Point] -> Double -> Double
    acc []                                   _   = 0
    acc [otherPoint]                         res = dist otherPoint point + res
    acc (firstPoint:coords'@(secondPoint:_)) res = acc coords' (dist firstPoint secondPoint + res)

-- | Calculate doubled area of given polygon, which is represented by list of its points.
-- Quite slow. For testing purposes only.
doubleAreaNaive :: [Point] -> Int
doubleAreaNaive []      = 0
doubleAreaNaive coords@(point:_) = abs $ acc coords 0
  where
    acc :: [Point] -> Int -> Int
    acc []                                   _   = 0
    acc [otherPoint]                         res = crossProduct otherPoint point + res
    acc (firstPoint:coords'@(secondPoint:_)) res = acc coords' (crossProduct firstPoint secondPoint + res)
