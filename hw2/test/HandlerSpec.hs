{-# OPTIONS_GHC -fno-warn-type-defaults #-}

module HandlerSpec
       ( spec
       ) where

import Test.Hspec

import Command.Handler
import FileSystem.FileSystem
import System.FilePath (pathSeparator)

import qualified Data.ByteString.Char8 as B
import qualified Data.Text as T

spec :: Spec
spec = do
  describe "'dir' command" $ do
    it "shows names of dirctories and files" $ do
      let innerDirectory = createEmptyDirectory "b" [[pathSeparator] ++ "a" ++ [pathSeparator], "b"]
      let root = Directory [(Left innerDirectory)] "a" [[pathSeparator] ++ "a"] defaultPermissions
      handleDirCommand root `shouldBe` "b\n"

  describe "'cat' command" $ do
    it "shows content of given file in current directory" $ do
      let innerFile = createEmptyFile "b" ".txt" ([pathSeparator] ++ "a" ++ [pathSeparator] ++ "b.txt")
      let root = Directory [(Right innerFile)] "a" [[pathSeparator] ++ "a"] defaultPermissions
      handleCatCommand "b.txt" root `shouldBe` Just (Right T.empty)

    it "returns nothing on invalid name" $ do
      let innerFile = createEmptyFile "b" ".txt" ([pathSeparator] ++ "a" ++ [pathSeparator] ++ "b.txt")
      let root = Directory [(Right innerFile)] "a" [[pathSeparator] ++ "a"] defaultPermissions
      handleCatCommand "c.txt" root `shouldBe` Nothing

  describe "'create-folder' command" $ do
    it "creates folder with given name in current directory" $ do
      let root = Directory [] "a" [[pathSeparator] ++ "a"] defaultPermissions
      let innerDirectory = createEmptyDirectory "b" [[pathSeparator] ++ "a" ++ [pathSeparator], "b"]
      let updatedRoot = Directory [(Left innerDirectory)] "a" [[pathSeparator] ++ "a"] defaultPermissions
      handleCreateFolderCommand "b" root `shouldBe` Just (updatedRoot)

    it "returns nothing if folder exists" $ do
      let innerDirectory = createEmptyDirectory "b" [[pathSeparator] ++ "a" ++ [pathSeparator], "b"]
      let root = Directory [(Left innerDirectory)] "a" [[pathSeparator] ++ "a"] defaultPermissions
      handleCreateFolderCommand "b" root `shouldBe` Nothing

  describe "'create-file' command" $ do
    it "creates file with given name in current directory" $ do
      let root = Directory [] "a" [[pathSeparator] ++ "a"] defaultPermissions
      let innerFile = createEmptyFile "b" ".txt" ([pathSeparator] ++ "a" ++ [pathSeparator] ++ "b.txt")
      let updatedRoot = Directory [(Right innerFile)] "a" [[pathSeparator] ++ "a"] defaultPermissions
      handleCreateFileCommand "b.txt" root  `shouldBe` Just (updatedRoot)

    it "returns nothing if file exists" $ do
      let innerFile = createEmptyFile "b" ".txt" ([pathSeparator] ++ "a" ++ [pathSeparator] ++ "b.txt")
      let root = Directory [(Right innerFile)] "a" [[pathSeparator] ++ "a"] defaultPermissions
      handleCreateFileCommand "b.txt" root `shouldBe` Nothing


  describe "'remove' command" $ do
    it "removes file with given name in current directory" $ do
      let innerFile = createEmptyFile "b" ".txt" ([pathSeparator] ++ "a" ++ [pathSeparator] ++ "b.txt")
      let root = Directory [(Right innerFile)] "a" [[pathSeparator] ++ "a"] defaultPermissions
      let updatedRoot = Directory [] "a" [[pathSeparator] ++ "a"] defaultPermissions
      handleRemoveCommand "b.txt" root `shouldBe` Just (updatedRoot)

    it "removes nothing if file not exists" $ do
      let root = Directory [] "a" [[pathSeparator] ++ "a"] defaultPermissions
      handleRemoveCommand "b.txt" root `shouldBe` Nothing

    it "removes folder with given name in current directory" $ do
      let innerDirectory = createEmptyDirectory "b" [[pathSeparator] ++ "a" ++ [pathSeparator], "b"]
      let root = Directory [(Left innerDirectory)] "a" [[pathSeparator] ++ "a"] defaultPermissions
      let updatedRoot = Directory [] "a" [[pathSeparator] ++ "a"] defaultPermissions
      handleRemoveCommand "b" root `shouldBe` Just (updatedRoot)

    it "removes nothing if folder not exists" $ do
      let root = Directory [] "a" [[pathSeparator] ++ "a"] defaultPermissions
      handleRemoveCommand "b" root `shouldBe` Nothing

  describe "'write-file' command" $ do
    it "writes in file with given name in current directory" $ do
      let innerFile = createEmptyFile "b" ".txt" ([pathSeparator] ++ "a" ++ [pathSeparator] ++ "b.txt")
      let root = Directory [(Right innerFile)] "a" [[pathSeparator] ++ "a"] defaultPermissions
      let byteText = B.pack "aa"
      let updatedFile = replaceContentInFile innerFile byteText
      let updatedRoot = Directory [(Right updatedFile)] "a" [[pathSeparator] ++ "a"] defaultPermissions
      handleWriteFileCommand "b.txt" byteText root `shouldBe` Just (updatedRoot)

    it "returns nothing if file not exist" $ do
      let innerFile = createEmptyFile "b" ".txt" ([pathSeparator] ++ "a" ++ [pathSeparator] ++ "b.txt")
      let root = Directory [(Right innerFile)] "a" [[pathSeparator] ++ "a"] defaultPermissions
      let byteText = B.pack "aa"
      handleWriteFileCommand "c.txt" byteText root `shouldBe` Nothing

  describe "'find-file' command" $ do
    it "find file with given name in current directory" $ do
      let innerFile = createEmptyFile "b" ".txt" ([pathSeparator] ++ "a" ++ [pathSeparator] ++ "b.txt")
      let root = Directory [(Right innerFile)] "a" [[pathSeparator] ++ "a"] defaultPermissions
      handleFindFileCommand "b.txt" root `shouldBe` Just (innerFile)

    it "find file with given name in sub directory of current" $ do
      let innerFile = createEmptyFile "c" ".txt" ([pathSeparator] ++ "a" ++ [pathSeparator] ++ "b" ++ [pathSeparator] ++ "c.txt")
      let innerDirectory = Directory [(Right innerFile)] "b" [[pathSeparator] ++ "a" ++ [pathSeparator], "b"] defaultPermissions
      let root = Directory [(Left innerDirectory)] "a" [[pathSeparator] ++ "a"] defaultPermissions
      handleFindFileCommand "c.txt" root `shouldBe` Just (innerFile)

    it "returns nothing if file not exist" $ do
      let innerFile = createEmptyFile "b" ".txt" ([pathSeparator] ++ "a" ++ [pathSeparator] ++ "b.txt")
      let root = Directory [(Right innerFile)] "a" [[pathSeparator] ++ "a"] defaultPermissions
      handleFindFileCommand "c.txt" root `shouldBe` Nothing

  describe "'information' command" $ do
    it "returns information about file" $ do
      let innerFile = createEmptyFile "b" ".txt" ([pathSeparator] ++ "a" ++ [pathSeparator] ++ "b.txt")
      let info = "File 'b' information:\n"
            ++ "  Path: " ++ [pathSeparator] ++ "a" ++ [pathSeparator] ++ "b.txt" ++ "\n"
            ++ "  Extension: .txt\n"
            ++ "  Last update time (UTC): 1858-11-17 00:00:00 UTC\n"
            ++ "  Size: 0\n"
            ++ "  Permissions: " ++ (show $ defaultPermissions) ++ "\n"
      handleInformationFileCommand innerFile `shouldBe` info

    it "returns information about folder" $ do
      let innerDirectory = createEmptyDirectory "b" [[pathSeparator] ++ "a" ++ [pathSeparator], "b"]
      let info = "Directory 'b' information:\n"
            ++ "  Path: " ++ [pathSeparator] ++ "a" ++ [pathSeparator] ++ "b"  ++ "\n"
            ++ "  Files count: 0\n"
            ++ "  Size: 0\n"
            ++ "  Permissions: " ++ (show $ defaultPermissions) ++ "\n"
      handleInformationFolderCommand innerDirectory `shouldBe` info
