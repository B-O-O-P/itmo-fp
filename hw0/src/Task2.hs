{-# LANGUAGE TypeOperators #-}

module Task2
  ( doubleNeg
  , excludedNeg
  , pierce
  , doubleNegElim
  , thirdNegElim
  ) where

import Data.Void (Void)

type Neg a = a -> Void

-- | law of counterposition
counterPosition :: (a -> b) -> (Neg b -> Neg a)
counterPosition f g a = g (f a)

-- | 6-th axiom of lambda calculus
axiom6 :: a -> Either a b
axiom6 = Left

-- | 7-th axiom of lambda calculus.
axiom7 :: b -> Either a b
axiom7 = Right

-- | 9-th axiom of lambda calculus.
axiom9 :: (a -> b) -> (a -> Neg b) -> Neg a
axiom9 f g a = g a (f a)

-- | law (a -> ¬¬a).
-- a -> (a -> Void) -> Void
doubleNeg :: a -> Neg (Neg a)
doubleNeg a f = f a

-- | ¬¬(a ∨ ¬a) 
-- ((a + (a -> Void)) -> Void) -> Void
excludedNeg :: Neg (Neg (Either a (Neg a)))
excludedNeg = axiom9 (counterPosition axiom6) (counterPosition axiom7)

-- | Pierce law.
-- This type can't be inferred.
-- It can be proofed by Kripke model:
--
--         w3 <- w1 -> w2
-- w2: a, w3: b
--
-- In this model ((a -> b) -> a) -> a will be not true in w1.
pierce :: ((a -> b) -> a) -> a
pierce = undefined

-- | The removal of the double negation. 
-- (( a - > Void) -> Void) -> a
-- This type can't be inferred.
-- It can be proofed by Kripke model:
--
--           w1 -> w2
-- w2: a
--
-- In this model (¬¬a -> a) will fail in w1.
doubleNegElim :: Neg (Neg a) -> a
doubleNegElim = undefined

-- | The removal of the triple negation.
-- (((a -> Void) -> Void) -> Void) -> a -> Void
thirdNegElim :: Neg (Neg (Neg a)) -> Neg a
thirdNegElim f x = f (doubleNeg x)
