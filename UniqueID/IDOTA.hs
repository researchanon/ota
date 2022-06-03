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
import OTA


atomCAS :: Eq a => IORef a -> a -> a -> IO Bool
atomCAS ptr old new =
   atomicModifyIORefCAS ptr (\ cur -> if cur == old
                                   then (new, True)
                                   else (cur, False))

type IDGen = IORef Int

newID :: IO IDGen
newID = newIORef 0

getIDCAS ::  IDGen -> IO Int
getIDCAS idg = do
  v <- readIORef idg
  ok <- atomCAS idg v (v+1)
  if ok then return (v+1) else getIDCAS idg

getID :: IDGen -> STM Int
getID idg = performIO $ liftIO $  getIDCAS idg


createThread :: Int -> IORef Int -> MVar Int -> IO ThreadId
createThread numOps ioValue mvar = forkIO (do
	callNTimes numOps (do
		x<-atomically $ getID  ioValue
		return x)
        putMVar mvar 1)

createThreads :: Int -> Int -> IORef Int -> [MVar Int] -> IO()
createThreads n numOps ioVar mvars = mapM_ (createThread numOps ioVar) mvars


main1 :: Int -> Int -> IO ()
main1 numops numThreads = do
    theSharedInt <- newID
    start <- getCurrentTime
    mvars <- replicateM numThreads newEmptyMVar
    let ops = div numops numThreads
	-- print(numops)
    threads <- createThreads numThreads ops theSharedInt mvars
    mapM_ takeMVar mvars
    stop <- getCurrentTime
    let time =  (read . reverse . tail . reverse . show) (diffUTCTime stop start) :: Double
    putStrLn $ "IDOTA\t" ++ show numThreads ++ "\t" ++show time
 


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
