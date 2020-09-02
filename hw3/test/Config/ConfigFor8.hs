module Config.ConfigFor8
  ( defaultParameters
  , defaultRadius
  ) where

import Task8 (InfectionParameter(..), Periods(..))

defaultParameters :: InfectionParameter
defaultParameters = InfectionParameter 0.5 (Periods 3 3 3)

defaultRadius :: Int
defaultRadius = 7
