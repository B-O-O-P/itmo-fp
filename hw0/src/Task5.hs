module Task5
  ( Nat
  , churchMult
  , churchPlus
  , churchToInt
  , succChurch
  , zero
  ) where

type Nat a = (a -> a) -> a -> a

zero :: Nat a
zero _ x = x

-- | Increment of Church number.
-- λn. λf. λx. f ( n f x)
-- Here (n f x) is function f applied to x n times
-- As need (n + 1), then f (n f x).
succChurch :: Nat a -> Nat a
succChurch n = \f x -> f (n f x)

-- | Sum of two Church numbers.
-- λa. λb. λf . λx. a f (b f x)
-- Same as succ, but now adding second number instead of one.
churchPlus :: Nat a -> Nat a -> Nat a
churchPlus a b = \f x -> a f (b f x)

-- | Multiplication of two Church numbers.
-- λa. λb. λf. λx. a (b f) x
-- Same as plus, but the function applied several times must not be f,
-- But a function that applies n times f.
churchMult :: Nat a -> Nat a -> Nat a
churchMult a b = \f x -> a (b f) x

-- | Convert Church number to Integer. 
-- Adding one n times to zero.
churchToInt :: Nat Integer -> Integer
churchToInt n = n (+1) 0
