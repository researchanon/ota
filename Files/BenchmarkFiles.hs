module Main where

import Control.Concurrent
import Control.Concurrent.STM
import Control.Concurrent.MVar
import System.Environment
import System.Random
--import Data.Time.Clock
import Data.Time
import Data.List
-- import Control.Concurrent.STM
import TBFilesORW
import Data.List.Split
import GHC.IO.IOMode


data Ops = Goto Int | Reverse Int 


genListOps :: Int -> Int -> IO [Ops]
genListOps range 0 = return []
genListOps range size = do
  op <- randomRIO (1,5::Int)
  val <- randomRIO (1,range)
  list <- genListOps range (size-1)
  case op of
    1 -> return (Goto val:list)
    2 -> return (Goto val:list)
    3 -> return (Goto val:list)
    4 -> return (Goto val:list)
    5 -> return (Reverse val:list)

genListOpsHeavy :: Int -> Int -> IO [Ops]
genListOpsHeavy range 0 = return []
genListOpsHeavy range size = do
  op <- randomRIO (1,5::Int)
  val <- randomRIO (1,range)
  list <- genListOpsHeavy range (size-1)
  case op of
    1 -> return (Goto val:list)
    2 -> return (Goto val:list)
    3 -> return (Reverse val:list)
    4 -> return (Reverse val:list)
    5 -> return (Reverse val:list)



main :: IO ()
main = do
  args <- getArgs
  let range = read (args!!0)
  let ops = read (args!!1)
  let heavy = read (args !! 2)
  let nthreads = read (args !! 3)
  let ops2 = div ops nthreads
  mvar <- newEmptyMVar
  listOps <- if heavy == 0
    then do
           list<- genListOps range ops
           return $ divvy ops2 ops2 list 
    else do
           list<- genListOpsHeavy range ops
           return $ divvy ops2 ops2 list
  assert (length listOps == nthreads) (do
      start <- getCurrentTime
      mapM (\ lops -> forkIO (thread mvar lops)) listOps
      mapM takeMVar (replicate nthreads mvar)
      stop <- getCurrentTime
      print $ diffUTCTime stop start)


assert :: Bool -> a -> a
assert False x = error "assertion failed!"
assert _     x = x

thread :: MVar () -> [Ops]-> IO ()
thread mvar [] = putMVar mvar ()
thread mvar (op:xs) = do
  case op of
    Goto val-> do
            atomically $ goto val
            thread mvar xs
    Reverse val -> do
            atomically $ reverseLine val
                       
            thread mvar xs

      

goto :: Int -> STM () 
goto lines = do
        hdl <- openFileSTM "test.txt" ReadMode
        readLines lines hdl

reverseLine :: Int -> STM ()
reverseLine line = do
     hdl <- openFileSTM "test.txt" ReadWriteMode
     readLines (line-1) hdl
     pos <- hTellSTM hdl
     line <- readLineSTM hdl
     hSeekSTM hdl pos
     writeLineSTM hdl (reverse line)





readLines :: Int -> THandle -> STM ()
readLines 0 hdl = return ()
readLines n hdl = do
                readLineSTM hdl
                readLines (n-1) hdl 
