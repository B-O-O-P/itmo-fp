module Task8Spec
  ( spec
  ) where

import Test.Hspec (Spec, describe, it)
import System.Random (newStdGen)

import Task8
import Utils.Grid (PrintableGrid(..))

import Config.ConfigFor8


performSteps :: Int -> InfectionParameter -> Grid Person -> Grid Person
performSteps 0 _ g = g
performSteps n p g = nextSimulation p (performSteps (n-1) p g)

spec :: Spec
spec = do
  describe "simulation" $ do
    it "creates grid with one incubation person" $ do
      gen <- newStdGen
      let initGrid = initSimulation defaultParameters gen
      putStrLn $ show $ PrintableGrid initGrid defaultRadius

    it "perfoms 7 consecutive steps with p = 0.5 and periods = 3, 3, 3" $ do
      gen <- newStdGen
      let initGrid = initSimulation defaultParameters gen
      let resGrid = performSteps 7 defaultParameters initGrid
      putStrLn $ show $ PrintableGrid resGrid defaultRadius

    it "perfoms 10 consecutive steps with p = 0.5 and periods = 3, 3, 3" $ do
          gen <- newStdGen
          let initGrid = initSimulation defaultParameters gen
          let resGrid = performSteps 10 defaultParameters initGrid
          putStrLn $ show $ PrintableGrid resGrid defaultRadius
