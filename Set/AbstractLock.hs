module AbstractLock where

import Control.Concurrent(myThreadId)
import HashTable.HashSet as HashSet
import Data.IORef
import Data.Atomics
import Control.Concurrent.STM

data ALock a = ALock (HTable a Lock)

data Lock = Lock (IORef Integer) (IORef Integer) deriving Eq

mytid2 :: IO Integer
mytid2 = do
  x <- myThreadId
  return ((read . drop 9) (show x))

atomCAS :: Eq a => IORef a -> a -> a -> IO Bool
atomCAS ptr old new =
  atomicModifyIORefCAS ptr (\ cur -> if cur == old
                                  then (new, True)
                                  else (cur, False))

newALock :: Int -> (a-> Int) ->  IO (ALock a)
newALock size f = do
  hasht <- newHash size f
  return (ALock hasht)

plusOne :: IORef Integer -> IO ()
plusOne ref = do
  v <-readIORef ref
  writeIORef ref (v+1)

decCounter:: IORef Integer -> IO ()
decCounter ref = do
  v <-readIORef ref
  writeIORef ref (v-1)

lock :: Eq a => ALock a -> a -> IO Bool
lock alock@(ALock ht) key = do
  mior <- HashSet.contains ht key
  myId <- mytid2
  case mior of
    Just (Lock ior counter) -> do
      v <- readIORef ior
      if (v == 0)
        then do
          locked <- atomCAS ior 0 myId
          case locked of
            True -> do
              plusOne counter
              return True
            False -> do
              return False
      else if (v == myId)
        then do
          plusOne counter
          return True
        else do
          return False
    Nothing -> do
      ior <- newIORef myId
      counter <- newIORef 1
      ok <- insert ht key (Lock ior counter)
      if ok
        then do
          return True
        else (lock alock key)

unlock :: Eq a => ALock a -> a -> IO Bool
unlock alock@(ALock ht) key = do
  mior <- HashSet.contains ht key
  myId <- mytid2
  case mior of
    Just (Lock ior counter) -> do
      nlocks <- readIORef counter
      if (nlocks > 1)
        then do
          decCounter counter
          return True
        else do
          decCounter counter
          ok <- atomCAS ior myId 0
          return ok
    Nothing -> error "Nothing unlock"
