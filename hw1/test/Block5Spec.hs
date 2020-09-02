module Block5Spec
       ( spec 
       ) where

import Test.Hspec

import Block5

spec :: Spec
spec = do
  describe "Task 1: Monadic caluclations" $ do
    it "Constant returns Int" $ do
      eval (Const (-1)) `shouldBe` Right (-1)
      eval (Const 2)    `shouldBe` Right 2

    it "Correct calculations" $ do
      eval (BinaryOp Add (Const 3) (Const 9))  `shouldBe` Right 12
      eval (BinaryOp Sub (Const 10) (Const 7)) `shouldBe` Right 3
      eval (BinaryOp Mul (Const 2) (Const 5))  `shouldBe` Right 10
      eval (BinaryOp Div (Const 6) (Const 2))  `shouldBe` Right 3
      eval (BinaryOp Pow (Const 2) (Const 3))  `shouldBe` Right 8

    it "Division by zero is error" $ do
      eval (BinaryOp Div (Const (-5)) (Const 0)) `shouldBe` Left DivisionByZero

    it "Negative power is error" $ do
      eval (BinaryOp Pow (Const 2) (Const (-1))) `shouldBe` Left NegativePower

  describe "Task 2: SMA" $ do
    it "Computes right average" $ do
      moving 4 [1, 5, 3, 8, 7, 9, 6] `shouldBe` [1.0, 3.0, 3.0, 4.25, 5.75, 6.75, 7.5]
      moving 2 [1, 5, 3, 8, 7, 9, 6] `shouldBe` [1.0, 3.0, 4.0, 5.5, 7.5, 8.0, 7.5]
