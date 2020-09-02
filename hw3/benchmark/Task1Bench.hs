module Task1Bench
    ( doubleAreaBench
    , perimeterBench
    ) where

import Criterion.Main (bench, bgroup, defaultMain, nf)
import Test.QuickCheck (generate)

import Task1
import Utils.Task1BenchUtils

doubleAreaBench :: Int -> IO ()
doubleAreaBench n = do
  generatedList <- generate $ generatePointList n
  defaultMain
    [ bgroup "doubleArea naive" [ bench ((show n) <> " points") $ nf doubleAreaNaive generatedList ]
    , bgroup "doubleArea"       [ bench ((show n) <> " points") $ nf doubleArea      generatedList ]
    ]

perimeterBench :: Int -> IO ()
perimeterBench n = do
  generatedList <- generate $ generatePointList n
  defaultMain
    [ bgroup "perimeter naive" [ bench ((show n) <> " points") $ nf perimeterNaive generatedList ]
    , bgroup "perimeter"       [ bench ((show n) <> " points") $ nf perimeter      generatedList ]
    ]
