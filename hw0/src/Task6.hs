module Task6
  ( foo
  , wnhfA
  , wnhfB
  ) where

import Data.Maybe (mapMaybe)

import Task1 (distributivity)

-- | Constructs pair of two equal strings.
--  (Left ("harold" ++ " hide " ++ "the " ++ "pain")), Left ("harold" ++ " hide " ++ "the " ++ "pain")))
-- wnhf = (Left ("harold" ++ " hide " ++ "the " ++ "pain"))
--        ,Left ("harold" ++ " hide " ++ "the " ++ "pain")
--        ) = (_, _) - it's constructor
wnhfA :: (Either String a, Either String b)
wnhfA = distributivity (Left ("harold" ++ " hide " ++ "the " ++ "pain"))

-- | Checks if string has "o" symbol.
--  null $ mapMaybe foo "pole chudes ochen' chudesno"
-- -> null $ mapMaybe foo "pole chudes ochen' chudesno"
-- -> null $ mapMaybe foo "ole chudes ochen' chudesno"
-- -> null $ (exp pi : mapMaybe foo "le chudes ochen' chudesno")
-- -> False
-- wnhf = False - primitive
wnhfB :: Bool
wnhfB = null $ mapMaybe foo "pole chudes ochen' chudesno"

foo :: Char -> Maybe Double
foo char =
  case char == 'o' of
    True  -> Just $ exp pi
    False -> Nothing
