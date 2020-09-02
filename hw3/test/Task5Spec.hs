module Task5Spec
  ( spec
  ) where

import Test.Hspec (Spec, describe, it, shouldBe)
import Lens.Micro ((^.))

import Task5

import Config.ConfigFor567 (testFS, testFSPath)

spec :: Spec
spec = do
  describe "scanFS" $ do
    it "creates valid FS" $ do
      scannedFS  <- scanFS testFSPath
      scannedFS `shouldBe` testFS

  describe "lenses" $ do
    it "name returns valid name" $ do
      scannedFS  <- scanFS testFSPath
      scannedFS^.name `shouldBe` "testFS"

    it "contents returns valid content" $ do
      scannedFS  <- scanFS testFSPath
      scannedFS^.contents `shouldBe` (_contents testFS)
