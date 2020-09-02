module Task7Spec
  ( spec
  ) where

import Test.Hspec (Spec, describe, it, shouldBe)
import Lens.Micro ((^?))

import Task5 (FS(..), scanFS)
import Task6 (file)
import Task7

import Config.ConfigFor567

spec :: Spec
spec = do
  describe "replace files extenstion in dir" $ do
    it "changes files extensions" $ do
      scannedFS  <- scanFS testFSPath
      scannedFS ^? file "testFile.java" `shouldBe` Nothing
      (replaceFilesExt scannedFS ".java")^? file "testFile.java" `shouldBe` (Just "testFile.java")

  describe "resursive ls" $ do
    it "returns all dirs and files recursively" $ do
      scannedFS  <- scanFS testFSPath
      lsRecursive scannedFS `shouldBe` ["testSubDir", "testFile", "testSubFile"]

  describe "file" $ do
    it "removes empty dir" $ do
      let scannedFS  = testFSWithEmptyDir
      removeIfEmpty scannedFS "emptyDir" `shouldBe` (Dir "withEmpty" [])

    it "removes nothing if dir is not empty" $ do
      scannedFS  <- scanFS testFSPath
      removeIfEmpty scannedFS "testSubDir" `shouldBe` testFS
