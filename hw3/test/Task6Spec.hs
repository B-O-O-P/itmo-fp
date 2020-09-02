module Task6Spec
  ( spec
  ) where

import Test.Hspec (Spec, describe, it, shouldBe)
import Lens.Micro ((^?), (^..))

import Task5 (scanFS)
import Task6

import Config.ConfigFor567

spec :: Spec
spec = do
  describe "cd" $ do
    it "changes to valid path" $ do
      scannedFS  <- scanFS testFSPath
      scannedFS ^? cd "testSubDir" `shouldBe` (Just testFSSubDir)

    it "doesn't change to invalid path" $ do
      scannedFS  <- scanFS testFSPath
      scannedFS ^? cd "invalidDir" `shouldBe` Nothing

  describe "ls" $ do
    it "returns all dirs and files" $ do
      scannedFS  <- scanFS testFSPath
      scannedFS ^.. ls `shouldBe` ["testSubDir", "testFile"]

    it "works with cd" $ do
      scannedFS  <- scanFS testFSPath
      scannedFS ^.. cd "testSubDir" . ls `shouldBe` ["testSubFile"]

  describe "file" $ do
    it "returns Just if file exists" $ do
      scannedFS  <- scanFS testFSPath
      scannedFS ^? file "testFile" `shouldBe` (Just "testFile")

    it "returns Nothing if file doesn't exist" $ do
      scannedFS  <- scanFS testFSPath
      scannedFS ^? file "invalidFile" `shouldBe` Nothing

    it "works with cd" $ do
      scannedFS  <- scanFS testFSPath
      scannedFS ^? cd "testSubDir" . file "testSubFile" `shouldBe` (Just "testSubFile")
