module Task3
  ( s
  , composition
  , identity
  , contraction
  , permutation
  ) where

-- this task is about SKI and BCKW basis

-- | S (2nd axiom).
s :: (a -> b -> c) -> (a -> b) -> a -> c
s f g x = f x (g x)

-- | K = const (1st axiom) - implementation of constant.
k :: a -> b -> a
k a _ = a

-- | B = S (K S) K - - implementation of composition function.
composition :: (b -> c) -> (a -> b) -> a -> c
composition = s (k s) k

-- | I = (S S (S K)) K - implementation of identity function.
identity :: a -> a
identity = s s (s k) k

-- | W = S S (S K) - implementation of contraction function.
contraction :: (a -> a -> b) -> a -> b
contraction = s s (s k)

-- | C = S (S (K (S (K S) K)) S) (K K) - permutation of 1-st and 2-nd arguments.
permutation :: (a -> b -> c) -> b -> a -> c
permutation = s (s (k (s (k s) k)) s) (k k)
