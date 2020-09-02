module Main where

import Task1Bench

main :: IO ()
main = do
  doubleAreaBench (10000000)
  perimeterBench  (10000000)
