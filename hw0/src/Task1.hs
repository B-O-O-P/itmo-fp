{-# LANGUAGE TypeOperators #-}

module Task1
  ( distributivity
  , associator
  , eitherAssoc
  ) where

-- | distribution law
-- a * ( b + c ) -> a * b + a * c
distributivity :: Either a (b, c) -> (Either a b, Either a c)
distributivity (Left a)      = (Left a, Left a)
distributivity (Right (b,c)) = (Right b, Right c)

-- | law of associativity
-- a * ( b * c ) -> (a * b ) * c
associator :: (a, (b, c)) -> ((a, b), c)
associator (a, (b, c)) = ((a, b), c)

type (<->) a b = (a -> b, b -> a)

-- | Either law of associativity 
-- a + ( b + c ) <=> ( a + b ) + c
eitherAssoc :: Either a (Either b c) <-> Either (Either a b) c
eitherAssoc = (leftAssoc, rightAssoc)
  where
    -- =>
    leftAssoc (Left a)          = (Left (Left a))
    leftAssoc (Right (Left b))  = (Left (Right b))
    leftAssoc (Right (Right c)) = (Right c)
    -- <=
    rightAssoc (Left (Left a))  = (Left a)
    rightAssoc (Left (Right b)) = (Right (Left b))
    rightAssoc (Right c)        = (Right (Right c))
