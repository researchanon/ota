module Main where

import Control.Concurrent
import GHC.Conc
import Control.Monad
import Data.Time
import System.Environment
import Control.Monad.IO.Class (MonadIO(..))
import GHC.IO (IO(..))
import Data.IORef
import Data.Atomics
import GHC.Conc.Sync



type IDGen = TVar Int

newID :: IO IDGen
newID = newTVarIO 0

getID ::  IDGen -> STM Int
getID idg = do
  v <- readTVar idg
  writeTVar idg (v+1)
  return (v+1)

createThread :: Int -> IDGen -> MVar Int -> IO ThreadId
createThread numOps ioValue mvar = forkIO ( do
	callNTimes numOps ( do
		x<- atomically $ getID  ioValue
		return x)
        putMVar mvar 1)

createThreads :: Int -> Int -> IDGen -> [MVar Int] -> IO()
createThreads n numOps ioVar mvars = mapM_ (createThread numOps ioVar) mvars


main1 :: Int -> Int -> IO ()
main1 numops numThreads = do
	theSharedInt <- newID
	start <- getCurrentTime
	mvars <- replicateM numThreads newEmptyMVar
	let ops = div numops numThreads
	threads <- createThreads numThreads ops theSharedInt mvars
	mapM_ takeMVar mvars
	stop <- getCurrentTime
	let time =  (read . reverse . tail . reverse . show) (diffUTCTime stop start) :: Double
        putStrLn $ "IDSTM\t" ++ show numThreads ++ "\t" ++show time
   --     putStrLn $ "IDSTM\t" ++ show numThreads ++ "\t" ++show (diffUTCTime stop start)
	


callNTimes :: Int -> IO a -> IO ()
callNTimes 0 _ = return ()
callNTimes times f = do
	x<-f
	seq x (callNTimes (times - 1) f)


main :: IO()
main = do
	args <- getArgs
	let numops = read (args!!0)
	let numthreads = read (args!!1)
	main1 numops numthreads
