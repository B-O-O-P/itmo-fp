module Block4Spec
       ( spec 
       ) where

import Test.Hspec

import Block4

spec :: Spec
spec = do
  describe "Task 1: stringSum" $ do
    it "returns correct sum if there is no errors" $ do
      stringSum "1"            `shouldBe` Just 1
      stringSum "1 2 3"        `shouldBe` Just 6
      stringSum ""             `shouldBe` Just 0
      stringSum "100 -422 322" `shouldBe` Just 0
      stringSum "2\n 4"        `shouldBe` Just 6
      
    it "returns Nothing on invalid input" $ do
      stringSum "1 help 4" `shouldBe` Nothing
      stringSum "3 + 3"    `shouldBe` Nothing