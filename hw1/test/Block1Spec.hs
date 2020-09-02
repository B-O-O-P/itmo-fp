{-# OPTIONS_GHC -fno-warn-type-defaults #-}

module Block1Spec
       ( spec
       ) where

import Test.Hspec
import Data.Maybe (isJust, isNothing)

import Block1
import Block3 (NonEmpty (..))

spec :: Spec
spec = do
  describe "Task 1: nextDay" $ do
    it "returns next day" $ do
      nextDay Monday    `shouldBe` Tuesday
      nextDay Tuesday   `shouldBe` Wednesday
      nextDay Wednesday `shouldBe` Thursday
      nextDay Thursday  `shouldBe` Friday
      nextDay Friday    `shouldBe` Saturday
      nextDay Saturday  `shouldBe` Sunday
      nextDay Sunday    `shouldBe` Monday

  describe "Task 1: afterDays" $ do
    it "returns same day if 0 on 7" $ do
      afterDays Monday 0 `shouldBe` Monday
      afterDays Monday 7 `shouldBe` Monday

    it "returns right day" $ do
      afterDays Monday 1   `shouldBe` Tuesday
      afterDays Monday 365 `shouldBe` Tuesday
      afterDays Sunday 2   `shouldBe` Tuesday
      afterDays Monday 4   `shouldBe` Friday


  describe "Task 1: isWeekend" $ do
    it "returns True on Sunday and Saturday" $ do
      isWeekend Sunday `shouldBe` True
      isWeekend Sunday `shouldBe` True

    it "returns False on other days" $ do
      isWeekend Monday    `shouldBe` False
      isWeekend Tuesday   `shouldBe` False
      isWeekend Wednesday `shouldBe` False
      isWeekend Thursday  `shouldBe` False
      isWeekend Friday    `shouldBe` False

  describe "Task 1: daysToParty" $ do
    it "returns right number left to Friday" $ do
      daysToParty Friday   `shouldBe` 0
      daysToParty Thursday `shouldBe` 1
      daysToParty Monday   `shouldBe` 4
      daysToParty Saturday `shouldBe` 6
      daysToParty Sunday   `shouldBe` 5

  describe "Task 2: Nat sum" $ do
    it "0 + x = x" $ do
      Z + Z       `shouldBe` Z
      Z + S (S Z) `shouldBe` S (S Z)

    it "succ x + y = succ (x + y)" $ do
      S Z         + S (S Z) `shouldBe` S (S (S Z))
      S (S (S Z)) + S (S Z) `shouldBe` S (S (S (S (S Z))))

    it "random tests" $ do
      (5 :: Nat)   + (5 :: Nat)   `shouldBe` (10 :: Nat)
      (2 :: Nat)   + (2 :: Nat)   `shouldBe` (4 :: Nat)
      (311 :: Nat) + (422 :: Nat) `shouldBe` (733 :: Nat)
      
  describe "Task 2: Nat mul" $ do 
    it "0 * x = 0" $ do
      Z * Z       `shouldBe` Z
      Z * S (S Z) `shouldBe` Z 
  
    it "succ x * y = x * y + x" $ do
       S Z         * S (S Z) `shouldBe` S (S Z)
       S (S (S Z)) * S (S Z) `shouldBe` S (S (S (S (S (S Z)))))    
       
    it "random tests" $ do
      (5 :: Nat)  * (5 :: Nat)  `shouldBe` (25 :: Nat)
      (2 :: Nat)  * (2 :: Nat)  `shouldBe` (4 :: Nat)
      (32 :: Nat) * (42 :: Nat) `shouldBe` (1344 :: Nat)
   
  describe "Task 2: Nat sub" $ do
    it "0 - x = 0 (0 is min element)" $ do
      Z - S Z `shouldBe` Z

    it "x - 0 = x" $ do
      S Z         - Z `shouldBe` S Z
      S (S (S Z)) - Z `shouldBe` S (S (S Z))

    it "succ x - succ y = x - y" $ do
      S (S Z)         - S Z `shouldBe` S Z
      S (S (S (S Z))) - S Z `shouldBe` S (S (S Z))

    it "random tests" $ do
      (5 :: Nat)   - (3 :: Nat)  `shouldBe` (2 :: Nat)
      (2 :: Nat)   - (2 :: Nat)  `shouldBe` (0 :: Nat)
      (322 :: Nat) - (42 :: Nat) `shouldBe` (280 :: Nat)

  describe "Task 2: converting" $ do
    it "convert Nat to Integer" $ do
       fromNat Z                 `shouldBe` 0
       fromNat (S Z)             `shouldBe` 1
       fromNat (S (S (S (S Z)))) `shouldBe` 4

    it "convert Integer to Nat" $ do
      (0 :: Nat) `shouldBe` Z
      (1 :: Nat) `shouldBe` (S Z)
      (4 :: Nat) `shouldBe` (S (S (S (S Z))))

    it "converting Nat to Integer and back must be same" $ do
      fromNat (5 :: Nat)   `shouldBe` 5
      fromNat (150 :: Nat) `shouldBe` 150

  describe "Task 2: Nat Equal" $ do
    it "returns True on same arguments" $ do
      Z       == Z       `shouldBe` True
      S (S Z) == S (S Z) `shouldBe` True

    it "returns False on different arguments" $ do
       S Z == Z       `shouldBe` False
       S Z == S (S Z) `shouldBe` False

    it "random tests" $ do
      (5 :: Nat)  == (4 :: Nat)  `shouldBe` False
      (15 :: Nat) == (15 :: Nat) `shouldBe` True

  describe "Task 2: Nat compare" $ do
    it "<= and >= returns True on same arguments" $ do
      Z <= Z `shouldBe` True
      Z >= Z `shouldBe` True

    it "returns LT(GT) if one argument is zero and other not" $ do
      Z   <  S Z `shouldBe` True
      S Z >  Z   `shouldBe` True
      Z   <= S Z `shouldBe` True
      S Z >= Z   `shouldBe` True

    it "succ x ? succ y = x ? y" $ do
       S Z     < S (S Z) `shouldBe` True
       S (S Z) > S Z     `shouldBe` True

    it "random tests" $ do
      (15 :: Nat) <  (15 :: Nat)   `shouldBe` False
      (5 :: Nat)  >  (4 :: Nat)    `shouldBe` True
      (3 :: Nat)  >= (3 :: Nat)    `shouldBe` True
      (99 :: Nat) >  (1000 :: Nat) `shouldBe` False

  describe "Task 2: isEven" $ do
    it "returns True on even number" $ do
      isEven Z           `shouldBe` True
      isEven (S (S Z))   `shouldBe` True
      isEven (10 :: Nat) `shouldBe` True
      isEven (36 :: Nat) `shouldBe` True

    it "returns False on odd number" $ do
      isEven (S Z)         `shouldBe` False
      isEven (S (S (S Z))) `shouldBe` False
      isEven (13 :: Nat)   `shouldBe` False
      isEven (35 :: Nat)   `shouldBe` False

  describe "Task 2: Nat div" $ do
    it "returns 0 if a < b" $ do
      S Z        // S (S Z)     `shouldBe` Z
      (5 :: Nat) // (10 :: Nat) `shouldBe` (0 :: Nat)

    it "random tests" $ do
      (1 :: Nat)   // (1 :: Nat)  `shouldBe` (1 :: Nat)
      (36 :: Nat)  // (9 :: Nat)  `shouldBe` (4 :: Nat)
      (322 :: Nat) // (23 :: Nat) `shouldBe` (14 :: Nat)

  describe "Task 2: Nat mod" $ do
    it "returns a if a < b" $ do
      S Z        % S (S Z)     `shouldBe` S Z
      (4 :: Nat) % (10 :: Nat) `shouldBe` (4 :: Nat)

    it "random tests" $ do
      (1 :: Nat)   % (1 :: Nat)  `shouldBe` (0 :: Nat)
      (36 :: Nat)  % (8 :: Nat)  `shouldBe` (4 :: Nat)
      (322 :: Nat) % (22 :: Nat) `shouldBe` (14 :: Nat)

  describe "Task 3: isEmpty" $ do
   it "returns True only on Leaf" $ do
    isEmpty Leaf                        `shouldBe` True
    isEmpty (Node (0 :| [0]) Leaf Leaf) `shouldBe` False

   it "returns False on not Leaf trees" $ do
    isEmpty (Node (1 :| [1, 1]) (Node (0 :| [0]) Leaf Leaf) Leaf)
      `shouldBe` False
    isEmpty (Node (2 :| [2, 2, 2]) Leaf (Node (1 :| []) (Node (0 :| []) Leaf Leaf) Leaf))
      `shouldBe` False

   describe "Task 3: size" $ do
    it "returns 0 if tree is Empty" $ do
      size Leaf `shouldBe` 0

    it "random tests" $ do
      size (Node (0 :| [0]) Leaf Leaf) `shouldBe` 2
      size (Node (2 :| [2, 2]) Leaf (Node (1 :| []) (Node (0 :| []) Leaf Leaf) Leaf)) `shouldBe` 5

  describe "Task 3: find(used in contains)" $ do
    it "returns False if element not found" $ do
      find Leaf 0
        `shouldSatisfy` (isNothing)
      find (Node (0 :| [0]) Leaf Leaf) 1
        `shouldSatisfy` (isNothing)
      find (Node (2 :| [2, 2]) Leaf (Node (1 :| []) (Node (0 :| []) Leaf Leaf) Leaf)) 3
        `shouldSatisfy` (isNothing)

    it "returns True if element found" $ do
      find (Node (0 :| [0]) Leaf Leaf) 0
              `shouldSatisfy` (isJust)
      find (Node (2 :| [2, 2]) (Node (1 :| []) (Node (0 :| []) Leaf Leaf) Leaf) Leaf) 1
              `shouldSatisfy` (isJust)

  describe "Task 3: insert" $ do
    it "result Tree contains added element" $ do
      find (insert Leaf 0) 0                        `shouldSatisfy` (isJust)
      find (insert (Node (0 :| [0]) Leaf Leaf) 1) 1 `shouldSatisfy` (isJust)

  describe "Task 3: fromList" $ do
    it "result Tree contains all elements from list" $ do
      find (fromList [0..3]) 0 `shouldSatisfy` (isJust)
      find (fromList [0..3]) 1 `shouldSatisfy` (isJust)
      find (fromList [0..3]) 2 `shouldSatisfy` (isJust)
      find (fromList [0..3]) 3 `shouldSatisfy` (isJust)

    it "result Tree doesn't contain other elements" $ do
      find (fromList [0..3]) 4 `shouldSatisfy` (isNothing)
      find (fromList [0..3]) 7 `shouldSatisfy` (isNothing)
