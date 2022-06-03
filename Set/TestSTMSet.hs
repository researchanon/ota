module Main where

import Control.Concurrent
import Control.Concurrent.MVar
import System.Environment
import System.Random
import GHC.Conc.Sync
import Data.List.Split
--import Data.Time.Clock
import Data.Time
import Data.List
-- import Control.Concurrent.STM
import STMSet
--import CASList
import GenOpsAndSet



fillSet :: Set Int -> [Int] -> IO ()
fillSet set [] = return ()
fillSet set (x:xs) = do
                  _ <- atomically $ add set x
                  fillSet set xs

main :: IO ()
main = do
  args <- getArgs
  let range = (read (args!!0)) :: Int
  let sizeSet = read (args!!1)
  let nops = read (args !!2)
  let nThreads = read (args !! 3)
  let opsFile = args !! 4
  let setFile = args !! 5
  let opsThread = div nops nThreads
  lops <- readOpsFile opsFile
  lset <- readSetFile setFile
  print("Initial set: " ++ show (length lset))
  print("Ops: " ++ show (length lops))
  assert (length lops == nops && length lset == sizeSet) 
    (do
      set <- newSet 
      fillSet set lset
      mvar <- newEmptyMVar
      let listDivided  = chunksOf opsThread lops
      let listOpsPerThread = if (length listDivided) > nThreads 
                                then assert (((length . head . reverse) listDivided)<nThreads) ((tail . reverse) listDivided) 
                                else listDivided 
      assert (length listOpsPerThread == nThreads)
        (do
          start <- getCurrentTime
          mapM (\ lops -> forkIO (thread set mvar lops)) listOpsPerThread
          mapM takeMVar (replicate nThreads mvar)
          stop <- getCurrentTime
          let time = (read . reverse . tail . reverse . show) (diffUTCTime stop start) :: Double
          putStrLn $ "STMSet\t" ++ show nThreads ++ "\t" ++show time))
         -- print $ diffUTCTime stop start))
          
      --    let time =  30 --(read . reverse . tail . reverse . show) (diffUTCTime stop start) :: Double
        --  putStrLn $ "STMSet\t" ++ show numThreads ++ "\t" ++show time)

thread :: Set Int -> MVar () -> [Op]-> IO ()
thread set mvar [] = putMVar mvar ()
thread set mvar (op:xs) = do
  case op of
    Contains val-> do
      _<- atomically $ contains set val
      thread set mvar xs
    Remove val -> do
      _<- atomically $ remove set val
      thread set mvar xs
    Add val -> do
      _<- atomically $ add set val
      thread set mvar xs
