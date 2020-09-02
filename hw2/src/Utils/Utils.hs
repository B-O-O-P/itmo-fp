module Utils.Utils
           ( sequenceIO
           , findJust
           , createZeroDay
           , modifyLastPath
           ) where

import Data.Time.Clock
import Data.Time.Calendar (Day(..))
import Control.Monad (foldM)
import System.FilePath (pathSeparator)

-- | Finds first not Nothing item in list. (I forgot about Alternative)
findJust :: [Maybe x] -> Maybe x
findJust []             = Nothing
findJust ((Just x) :_)  = Just x
findJust ((Nothing):xs) = findJust xs

makeListInIO :: [a] -> IO a -> IO [a]
makeListInIO xs act = do
  x <- act
  return $ xs ++ [x]

-- | Pulls out IO monad from list of IO items.
sequenceIO :: [IO a] -> IO [a]
sequenceIO xs = foldM makeListInIO [] xs

-- | Creates zero day in UTC time.
createZeroDay :: UTCTime
createZeroDay = do
  let zeroDay     = ModifiedJulianDay 0
  let zeroDayTime = secondsToDiffTime 0
  UTCTime zeroDay zeroDayTime

-- | Adds path separator in last directory of given path.
modifyLastPath :: [String] -> [String]
modifyLastPath path = do
  let pathLength = length path
  let result = (take (pathLength - 1) path) ++ [(last path ++ [pathSeparator])]
  result
