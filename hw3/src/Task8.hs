{-# LANGUAGE InstanceSigs #-}

module Task8
  ( InfectionParameter (..)
  , Grid (..)
  , Person (..)
  , InfectionStatus (..)
  , Periods(..)
  , initSimulation
  , nextSimulation
  ) where

import Control.Comonad (extend, extract)
import System.Random (StdGen , random, split)

import Utils.ListZipper (ListZipper(..), genericMove)
import Utils.Grid (Grid(..), gridWrite, gridRead, neighbours)

-- | ADT of probable health status of person.
data InfectionStatus
  = Healthy
  | Incubation
  | Symptoms
  | Immune
  deriving (Eq, Enum)

-- | Show instance used for representing status in Grid.
-- '_' - Healthy.
-- '%' - Incubation period.
-- '#' - Sick with symptoms.
-- '@' - Healthy and immune.
instance Show InfectionStatus where
  show :: InfectionStatus -> String
  show st = case st of
    Healthy    -> "_"
    Incubation -> "%"
    Symptoms   -> "#"
    Immune     -> "@"

-- | Change current infection status to the next stage.
infectionProgress :: InfectionStatus -> InfectionStatus
infectionProgress Immune = Healthy
infectionProgress st     = succ st

-- | Data type, which represents person by its health status and days till the next stage of infection.
-- Also contains random generator for simulation of infection.
data Person = Person
  { status    :: InfectionStatus
  , days      :: Int
  , generator :: StdGen
  }

-- | Default person representation.
defaultPerson :: StdGen -> Person
defaultPerson gen = Person Healthy 1 gen

-- | Data type for periods parameters. Contains all types of infection stages.
data Periods = Periods
  { incubation :: Int
  , symptoms   :: Int
  , immune     :: Int
  }

-- | Data type for parameters of simulation.
-- Contains infection probability and periods for all stages.
data InfectionParameter = InfectionParameter
  { probability :: Double
  , periods      :: Periods
  }

instance Show Person where
  show :: Person -> String
  show person = show $ status person

-- | Get next generator from given one.
nextGenerator :: StdGen -> StdGen
nextGenerator gen' = snd $ split gen'

-- | Return days for the next infection status.
nextPeriod :: InfectionStatus -> Periods -> Int
nextPeriod stat period = case stat of
  Incubation -> symptoms period
  Symptoms   -> immune period
  _          -> 1

-- | Change person infection status if it's expired or decrese number of days till expiration.
changeInfectionStatus :: InfectionParameter -> Person -> Person
changeInfectionStatus (InfectionParameter _ period) (Person stat periodDays gen) = updateInfectionStatus $ nextPeriod stat period
  where
    updateInfectionStatus :: Int -> Person
    updateInfectionStatus infectedDays =
      if periodDays == 0
        then Person (infectionProgress stat) infectedDays gen
      else Person stat (periodDays - 1) gen

-- | Calculcate infection probability using 'StdGen' based on number of infected neighbours.
-- Return pair of new 'StdGen' and status of infection.
chanceToInfect :: Int -> Double -> StdGen -> (StdGen, Bool)
chanceToInfect neighboursCount prob gen =
  if neighboursCount <= 0
    then (gen, False)
  else do
    let (p, newGen) = random gen
    if p <= prob
      then (newGen, True)
    else chanceToInfect (neighboursCount - 1) prob newGen

-- | Try to infect person. Return pair of new 'StdGen' and status of infection.
infection :: Double -> Grid Person -> (StdGen, Bool)
infection p g = do
  let gen = generator $ gridRead g
  let neightboursStatuses = map (\direction -> status $ extract $ direction g) neighbours
  let infectedNeighboursCount = (length . filter (\stat -> stat == Incubation || stat == Symptoms))
                                neightboursStatuses
  chanceToInfect infectedNeighboursCount p gen

-- | Rule for subsequent simulation steps.
rule :: InfectionParameter -> Grid Person -> Person
rule params@(InfectionParameter p period) g = do
  let currentPerson = gridRead g
  let stat = status currentPerson
  let periodDays = days currentPerson
  case stat of
    Healthy -> case infection p g of
      (gen', True) -> Person Incubation (incubation period) gen'
      (gen', False) -> Person stat periodDays gen'
    _ -> changeInfectionStatus params (gridRead g)

-- | Applies function to given person generator.
nextElem :: ((StdGen, StdGen) -> StdGen) -> (Person -> Person)
nextElem f (Person st d gen) = Person st d (f $ split gen)

-- | Get new infinite zipper from rows.
nextZipper :: ((StdGen, StdGen) -> StdGen) -> (ListZipper Person -> ListZipper Person)
nextZipper f (LZ _ (Person _ _ gen) _) = createRow (f $ split $ nextGenerator gen)

-- | Create infinity row by splitting one person generators in two and creates default person in center.
createRow :: StdGen -> ListZipper Person
createRow gen = genericMove (nextElem fst) (nextElem snd) (defaultPerson gen)

-- | Initial state of infection grid with one person in the center in incbation stage.
initSimulation :: InfectionParameter -> StdGen -> Grid Person
initSimulation (InfectionParameter _ period) gen = do
  let initGrid = Grid $ genericMove (nextZipper fst) (nextZipper snd) (createRow gen)
  gridWrite (Person Incubation (incubation period) gen) (initGrid)

-- | Perform next step of the simulation.
nextSimulation :: InfectionParameter -> Grid Person -> Grid Person
nextSimulation params = extend $ rule params
