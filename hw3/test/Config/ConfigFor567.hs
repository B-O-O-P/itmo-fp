module Config.ConfigFor567
  ( testFS
  , testFSSubDir
  , testFile
  , testSubFile
  , testFSPath
  , testFSWithEmptyDir
  ) where

import System.FilePath ((</>))

import Task5 (FS(..))

testFS :: FS
testFS = Dir "testFS" [testFSSubDir, testFile]

testFSSubDir:: FS
testFSSubDir = Dir "testSubDir" [testSubFile]

testFile :: FS
testFile = File "testFile"

testSubFile :: FS
testSubFile = File "testSubFile"

testFSWithEmptyDir :: FS
testFSWithEmptyDir = Dir "withEmpty" [Dir "emptyDir" []]

testFSPath :: FilePath
testFSPath = "test" </> "testFS"
