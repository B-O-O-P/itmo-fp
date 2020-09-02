module Block5
  ( Operation(..)
  , ArithmeticError(..)
  , Expr(..)
  , eval
  , computeSMA
  , dropIf
  , moving
  ) where

import Control.Monad.State (State, evalState, put, get)

-- Task 1

-- | ADT of arithmetic operators.
data Operation
  = Add
  | Sub
  | Mul
  | Div
  | Pow
  deriving (Show)

-- | Data type of arithmetic expressions.
data Expr
  = Const Int
  | BinaryOp Operation
             Expr
             Expr
  deriving (Show)

-- | ADT of arithmetic errors.
data ArithmeticError
  = DivisionByZero
  | NegativePower
  deriving (Show, Eq)

-- | Evaluates arithmetic expression.
eval :: Expr -> Either ArithmeticError Int
eval (Const a) = Right a
eval (BinaryOp op left right) = eval left >>= (\x -> eval right >>= binaryOp op x)
  where
    binaryOp :: Operation -> Int -> Int -> Either ArithmeticError Int
    binaryOp Add x y = Right (x + y)
    binaryOp Sub x y = Right (x - y)
    binaryOp Mul x y = Right (x * y)
    binaryOp Div x y
      | y == 0    = Left DivisionByZero
      | otherwise = Right (x `div` y)
    binaryOp Pow x y
      | y < 0     = Left NegativePower
      | otherwise = Right (x ^ y)


-- Task 2

-- | Implementation of the Simple Moving Average Algorithm.
computeSMA :: Int -> Double -> State [Double] Double
computeSMA period x = do
  previousValues <- get
  let values = previousValues ++ [x]
  let newAverage = 
                   if length values <= period
                    then (sum values) / (fromIntegral $ length remainingValues :: Double)
                    else (sum remainingValues) / (fromIntegral $ length remainingValues :: Double)
                      where remainingValues = dropIf period values
  put (dropIf period values)
  return newAverage

-- | Safe drop.
dropIf :: Int -> [a] -> [a]
dropIf x xs = drop ((length xs) - x) xs

-- | Call of SMA algorithm.
moving :: Int -> [Double] -> [Double]
moving period list = evalState (mapM (computeSMA period) list) []