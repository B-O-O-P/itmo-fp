module Block6Spec
       ( spec 
       ) where

import Test.Hspec
import Data.Char  (isUpper)
import Data.Maybe (isJust, isNothing)

import Block6

spec :: Spec
spec = do
  describe "Task 2: parser combinators" $ do
    it "ok always Just" $ do
      runParser ok "abc"    `shouldSatisfy` (isJust)
      runParser ok "xyz123" `shouldSatisfy` (isJust)

    it "eof checks end" $ do
      runParser eof ""    `shouldSatisfy` (isJust)
      runParser eof "abc" `shouldSatisfy` (isNothing)

    it "satisfy checks predicate" $ do
      runParser (satisfy isUpper) "abc" `shouldBe` Nothing
      runParser (satisfy isUpper) "ABC" `shouldBe` Just ('A',"BC")

    it "element combinator parses one char" $ do
      runParser (element 'a') "abc" `shouldBe` Just('a' ,"bc")
      runParser (element 'a') "xyz" `shouldBe` Nothing

    it "stream combinator parses string" $ do
      runParser (stream "ab") "abc" `shouldBe` Just("ab", "c")
      runParser (stream "ab") "xyz" `shouldBe` Nothing

  describe "Task 3: Simple parsers" $ do
    it "CBS parser on correct input" $ do
        runParser cbsParser "()"     `shouldSatisfy` (isJust)
        runParser cbsParser "(()())" `shouldSatisfy` (isJust)

    it "CBS parser on invalid input" $ do
       runParser cbsParser ")("   `shouldSatisfy` (isNothing)
       runParser cbsParser "()))" `shouldSatisfy` (isNothing)

    it "Int parser on correct input" $ do
      runParser intParser "228" `shouldBe` Just (228, "")
      runParser intParser "+1"  `shouldBe` Just (1, "")
      runParser intParser "-1"  `shouldBe` Just (-1, "")

    it "Int parser on invalid input" $ do
      runParser intParser "*1"  `shouldSatisfy` (isNothing)
      runParser intParser "--1" `shouldSatisfy` (isNothing)

  describe "Task 4: Not Simpler parser" $ do
    it "list of lists parser on correct input" $ do
      runParser listlistParser "+2 , -3, -9"           `shouldBe` Just ([[-3, -9]], "")
      runParser listlistParser "2, 1,+10  , 3,5,-7, 2" `shouldBe` Just ([[1, 10], [5, -7, 2]], "")
