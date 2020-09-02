{-# LANGUAGE BangPatterns #-}
module Task2Spec
  ( spec
  ) where

import Test.Hspec (Spec, describe, it, shouldBe)

import Control.Concurrent (forkIO, threadDelay)
import Control.Monad (forM_)

import Task2 (newCHT, putCHT, getCHT, sizeCHT)

spec :: Spec
spec = do
  describe "ConcurrentHashTable" $ do
    it "performs valid insertions in single thread" $ do
      let indexes = [1..1000] :: [Int]
      table <- newCHT
      forM_ indexes $ \i ->
        putCHT ("Index" <> (show i)) (i * 10) table
      size <- sizeCHT table
      size `shouldBe` 1000

    it "performs insertions in 2 threads" $ do
      let indexes = [1..100] :: [Int]
      table <- newCHT
      putStrLn "    LOG: "
      _threadId1 <- forkIO $ do
        forM_ indexes $ \i -> putCHT ("Index" <> (show $ i + 1)) (i * 10) table
        putStrLn "      First thread finished"
      _threadId2 <- forkIO $ do
        forM_ indexes $ \i -> putCHT ("Index" <> (show $ i + 2)) (i * 10) table
        putStrLn "      Second thread finished"
      threadDelay 1000000
      size <- sizeCHT table
      size `shouldBe` 200

    it "performs put/get in 2 threads" $ do
          let indexes = [1..100] :: [Int]
          table <- newCHT
          putStrLn "    LOG: "
          _threadId1 <- forkIO $ do
            forM_ indexes $ \i -> putCHT ("Index" <> (show $ i + 1)) (i * 10) table
            putStrLn "      First thread finished"
          _threadId2 <- forkIO $ do
            forM_ (reverse indexes) $ \i -> getCHT ("Index" <> (show $ i + 1)) table
            putStrLn "      Second thread finished"
          threadDelay 1000000
          size <- sizeCHT table
          size `shouldBe` 100
