{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import System.Console.ANSI (setSGR, SGR(..), ConsoleLayer(..), ColorIntensity(..), Color(..)
                           , clearFromCursorToScreenBeginning
                           , setCursorPosition)
import Control.Monad (forM_)
import Control.Concurrent (threadDelay)
import System.Random (newStdGen)

import Task8 (Person, InfectionParameter(..), Periods(..), initSimulation, nextSimulation)
import Utils.Grid (Grid(..), PrintableGrid(..))

main :: IO ()
main = do
  putStrLn $ "Main function is only for Comonad task."
  putStrLn $ "**COMONAD-19**"
  putStrLn $ "Input probability of infection - Double number:"
  (prob :: Double) <- readLn
  putStrLn $ "Input days for periods - incubation, with symptoms and immune(separated by space):"
  ps <- getLine
  let [inc, sym, im] = map read . words $ ps
  let params = InfectionParameter prob (Periods inc sym im)
  putStrLn $ "Input number of steps:"
  (steps :: Int) <- readLn
  gen <- newStdGen
  let initSim = initSimulation params gen
  performNiceOutput steps initSim params
  putStrLn $ "**That's all, folks! Bye-bye!**"



performNiceOutput :: Int -> Grid Person -> InfectionParameter -> IO ()
performNiceOutput 0 _ _ = return ()
performNiceOutput n g p = do
  performNiceStep (PrintableGrid g 7)
  threadDelay 100000
  performNiceOutput (n - 1) (nextSimulation p g) p

performNiceStep :: PrintableGrid Person -> IO ()
performNiceStep g = do
  clearFromCursorToScreenBeginning
  setCursorPosition 0 0
  let currentStepStr = show g
  forM_ currentStepStr (\st -> putStatus st)


putStatus :: Char -> IO ()
putStatus st = do
  case st of
    '#' -> setSGR [SetColor Foreground Dull Red]
    '@' -> setSGR [SetColor Foreground Dull Green]
    '%' -> setSGR [SetColor Foreground Dull Blue]
    _   -> setSGR [Reset]
  putChar $ st
