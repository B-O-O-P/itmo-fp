{-# LANGUAGE BangPatterns #-}

module Task2
    ( ConcurrentHashTable(..)
    , newCHT
    , getCHT
    , putCHT
    , sizeCHT
    ) where

import Control.Concurrent.STM (STM, TVar, atomically, readTVar, newTVar, modifyTVar', writeTVar)
import Data.Hashable (Hashable(..))
import Data.Foldable (forM_)
import qualified Data.Vector as V
import qualified Data.Map as M

-- | Type alias for Bucket, which is used for distributing key/value pair.
type Bucket k v = M.Map k v

-- | Data type for Concurrent Hashtable, which is represented by count of values and vector of 'Buckets'.
-- Uses O(1) + O(n) memory, where n - count/
data ConcurrentHashTable k v = ConcurrentHashTable
  { count :: TVar Int
  , getVector :: TVar (V.Vector (TVar (Bucket k v)))
  }

-- | Get the bucket from vector by calculating hash of given key.
bucket :: Hashable k => ConcurrentHashTable k v -> k -> STM (TVar (Bucket k v))
bucket h k = do
  vec <- readTVar $ getVector h
  return $! vec V.!(hash k `mod` (V.length vec))

-- | Create a new hashTable with fixed size.
newSTM :: Int -> STM (ConcurrentHashTable k v)
newSTM n = do
  count' <- newTVar 0
  vec <- V.replicateM n (newTVar M.empty)
  vecTVar <- newTVar vec
  return $! ConcurrentHashTable count' vecTVar

-- | Add value into given hashtable.
addSTM :: (Hashable k, Ord k) => k -> v -> ConcurrentHashTable k v -> STM ()
addSTM k v h = do
  b <- bucket h k
  m <- readTVar b
  let m' = M.insert k v m
  writeTVar b $! m'

-- | Insert a key/value pair into given hashtable. O(log n) - time complexty.
--  Uses 'expandCHT' if doubled count more than size of bucket.
putSTM :: (Hashable k, Ord k) => k -> v -> ConcurrentHashTable k v -> STM ()
putSTM !k !v h = do
  count' <- readTVar $ count h
  vec <- readTVar $ getVector h
  let bucketSize = V.length vec
  if (count' * 2 > bucketSize)
    then expandCHT h bucketSize
  else return ()
  addSTM k v h
  modifyTVar' (count h) (+1)

-- | Expand vector of given hashtable 2 times.
expandCHT :: (Hashable k, Ord k) => ConcurrentHashTable k v -> Int -> STM ()
expandCHT h len = do
  entry <- toList h
  newVec <- V.replicateM (2 * len) (newTVar M.empty)
  writeTVar (getVector h) newVec
  forM_ entry (\(k', v') ->
    addSTM k' v' h)

-- | Atomically create a new concurrent hashtable with fixed size(100).
newCHT :: IO (ConcurrentHashTable k v)
newCHT = atomically $ newSTM 100

-- | Atomically lookup a key in hashtable. O(log n) - time complexty
getCHT :: (Hashable k, Ord k) => k -> ConcurrentHashTable k v -> IO (Maybe v)
getCHT k h = do
  list <- atomically $ do
    b <- bucket h k
    readTVar b
  return $! M.lookup k list

-- | Atomically insert a key/value pair into hashtable. O(log n) - time complexty.
putCHT :: (Hashable k, Ord k) => k -> v -> ConcurrentHashTable k v -> IO ()
putCHT !k !v h = atomically $ putSTM k v h

-- | Atomically get size of given hashtable. O(1) - time complexty.
sizeCHT :: ConcurrentHashTable k v -> IO Int
sizeCHT h = do
  let c = count h
  atomically $ readTVar c

-- | Get a list of key/value pairs. O(n) - time complexty. For testing puposes only.
toList :: ConcurrentHashTable k v -> STM [(k, v)]
toList h = do
  vec <- readTVar $ getVector h
  bs <- buckets vec
  return $ concat $ map M.toList bs
    where
      buckets :: V.Vector (TVar b) -> STM [b]
      buckets v = mapM readTVar $ V.toList v
