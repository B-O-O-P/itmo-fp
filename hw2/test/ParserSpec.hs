{-# OPTIONS_GHC -fno-warn-type-defaults #-}

module ParserSpec
       ( spec
       ) where

import Test.Hspec

import Command.Parser
import Command.Command
import ProgramSettings (opts)

import qualified Data.Text as T

spec :: Spec
spec = do
  describe "parsing 'cd'" $ do
    it "succsess fully parses 'cd a'" $ do
      parsed <- parse "cd a" opts
      parsed `shouldBe` Just (CommandCd "a")

    it "returns nothing if missing name and shows help for command" $ do
      parsed <- parse "cd " opts
      parsed `shouldBe` Nothing

    it "returns nothing if more aguments thant need and shows help for command" $ do
      parsed <- parse "cd a a" opts
      parsed `shouldBe` Nothing

  describe "parsing 'dir'" $ do
    it "returns nothing if missing name and shows help for command" $ do
      parsed <- parse "dir" opts
      parsed `shouldBe` Just (CommandDir)

  describe "parsing 'ls'" $ do
    it "succsess fully parses 'ls a'" $ do
      parsed <- parse "ls a" opts
      parsed `shouldBe` Just (CommandLs "a")

    it "returns nothing if missing name and shows help for command" $ do
      parsed <- parse "ls " opts
      parsed `shouldBe` Nothing

    it "returns nothing if more aguments thant need and shows help for command" $ do
      parsed <- parse "ls a a" opts
      parsed `shouldBe` Nothing

  describe "parsing 'create-folder'" $ do
    it "succsess fully parses 'create-folder a'" $ do
      parsed <- parse "create-folder a" opts
      parsed `shouldBe` Just (CommandCreateFolder "a")

    it "returns nothing if missing name and shows help for command" $ do
      parsed <- parse "create-folder " opts
      parsed `shouldBe` Nothing

    it "returns nothing if more aguments thant need and shows help for command" $ do
      parsed <- parse "create-folder a a" opts
      parsed `shouldBe` Nothing

  describe "parsing 'create-file'" $ do
    it "succsess fully parses 'create-folder a'" $ do
      parsed <- parse "create-folder a" opts
      parsed `shouldBe` Just (CommandCreateFolder "a")

    it "returns nothing if missing name and shows help for command" $ do
      parsed <- parse "create-file " opts
      parsed `shouldBe` Nothing

    it "returns nothing if more aguments thant need and shows help for command" $ do
      parsed <- parse "create-file aa.txt a" opts
      parsed `shouldBe` Nothing

  describe "parsing 'remove'" $ do
    it "succsess fully parses 'remove a'" $ do
      parsed <- parse "remove a" opts
      parsed `shouldBe` Just (CommandRemove "a")

    it "succsess fully parses 'remove a.txt'" $ do
          parsed <- parse "remove a.txt" opts
          parsed `shouldBe` Just (CommandRemove "a.txt")

    it "returns nothing if missing name and shows help for command" $ do
      parsed <- parse "remove " opts
      parsed `shouldBe` Nothing

    it "returns nothing if more aguments thant need and shows help for command" $ do
      parsed <- parse "remove aa.txt a" opts
      parsed `shouldBe` Nothing

  describe "parsing 'write-file'" $ do
    it "succsess fully parses 'write-file a.txt hello'" $ do
      let text = T.pack "hello"
      parsed <- parse ("write-file a.txt hello") opts
      parsed `shouldBe` Just (CommandWriteFile "a.txt" text)

    it "returns nothing if missing name and shows help for command" $ do
      parsed <- parse "write-file hello" opts
      parsed `shouldBe` Nothing

    it "returns nothing if missing text and shows help for command" $ do
      parsed <- parse "write-file a.txt" opts
      parsed `shouldBe` Nothing

    it "returns nothing if more aguments thant need and shows help for command" $ do
      parsed <- parse "write-file aa.txt hello world" opts
      parsed `shouldBe` Nothing

  describe "parsing 'find-file'" $ do
    it "succsess fully parses 'find-file a'" $ do
      parsed <- parse "find-file a" opts
      parsed `shouldBe` Just (CommandFindFile "a")

    it "returns nothing if missing name and shows help for command" $ do
      parsed <- parse "find-file " opts
      parsed `shouldBe` Nothing

    it "returns nothing if more aguments thant need and shows help for command" $ do
      parsed <- parse "find-file a a" opts
      parsed `shouldBe` Nothing

  describe "parsing 'information'" $ do
    it "succsess fully parses 'information a'" $ do
      parsed <- parse "information a" opts
      parsed `shouldBe` Just (CommandInformation "a")

    it "succsess fully parses 'information a.txt'" $ do
          parsed <- parse "information a.txt" opts
          parsed `shouldBe` Just (CommandInformation "a.txt")

    it "returns nothing if missing name and shows help for command" $ do
      parsed <- parse "information " opts
      parsed `shouldBe` Nothing

    it "returns nothing if more aguments thant need and shows help for command" $ do
      parsed <- parse "information a a.txt " opts
      parsed `shouldBe` Nothing
