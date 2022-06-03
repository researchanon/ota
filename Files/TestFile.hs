module Main where


import GHC.Conc.Sync
import Control.Monad.IO.Class (MonadIO(..))
import GHC.IO (IO(..))
import           Control.Concurrent.ReadWriteLock        
import System.IO
import System.Environment
import System.Random
import TBFilesORW
import Control.Concurrent.MVar
import Data.Time
import Control.Concurrent

main = do
    args <- getArgs
    let nthreads = read (args !! 0)
    mvar <- newEmptyMVar
    start <- getCurrentTime
    mapM forkIO $ replicate nthreads (thread2 mvar)
    mapM takeMVar (replicate nthreads mvar)
    threadDelay 1000000
    stop <- getCurrentTime
    print $ diffUTCTime stop start

thread2 :: MVar () -> IO ()
thread2 mvar = do 
  atomically (do
    hdl <- openFileSTM "test.txt" ReadWriteMode
    line <- readLineSTM hdl
    performIO $ liftIO $ putStr line >>  putStr line >> putStr line)
  putMVar mvar ()
   
thread :: MVar () -> IO ()
thread mvar = do
          op <- randomRIO (0,9::Int)
          atomically $ trans op 
          putMVar mvar ()
 where
   trans lines = do
        hdl <- openFileSTM "test.txt" ReadWriteMode
        readLines lines hdl
        writeLineSTM hdl "987654321"
   readLines 0 hdl = return ()
   readLines n hdl = do
                readLineSTM hdl
                readLines (n-1) hdl 
                   


