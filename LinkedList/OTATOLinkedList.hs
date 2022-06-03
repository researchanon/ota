module OTATOLinkedList where

import NewCASList
import GHC.Conc.Sync
import Control.Monad.IO.Class (MonadIO(..))
--import GHC.IO (IO(..))
import Control.Concurrent.FairRWLock
import Data.IORef
import System.Timeout
import OTA


data ListHandle a = TBL RWLock (NewCASList.ListHandle a)

newList :: IO (OTATOLinkedList.ListHandle a)
newList = do
  --alock <- newALock size
  list <- NewCASList.newList
  rwl <- new
  return (TBL rwl list)

fromList :: NewCASList.ListHandle a -> IO (OTATOLinkedList.ListHandle a)
fromList list = do
              rwl <- new
              return (TBL rwl list)


add :: Eq a => OTATOLinkedList.ListHandle a -> a -> STM ()
add (TBL rwl list) key = runOT $ do
     ok <- liftIO $ tryAcquireWrite rwl
     if not ok then abort
               else do
                 ioref<-liftIO $ NewCASList.addToTail list key 
                 onCommit unlock
                 onAbort (undo ioref) 
  where  
    undo ioref = do
           NewCASList.delete list ioref
           releaseWrite rwl
           return ()
    unlock = releaseWrite rwl >> return ()

contains :: Eq a => OTATOLinkedList.ListHandle a -> a -> STM Bool
contains (TBL rwl list) key = 
  runOT $ do
     ok <- liftIO $ tryAcquireRead rwl
     if not ok then abort
               else do
            onCommit (releaseRead rwl >> return ())
            onAbort (releaseRead rwl >> return ())
            mnode <- liftIO $ NewCASList.find list key
            case mnode of
              Just x -> return True 
              Nothing -> return False    
 
 
remove :: Eq a => OTATOLinkedList.ListHandle a -> a -> STM Bool
remove (TBL rwl list) key = runOT $ do
     ok <- liftIO $ tryAcquireWrite rwl
     if not ok then abort
               else do
         mnode <- liftIO $ NewCASList.find list key
         case mnode of
             Just ioref -> do 
                  liftIO $ writeIORef ioref Nothing
                  onCommit (NewCASList.delete list ioref >> releaseWrite rwl >> return ())
                  onAbort (writeIORef ioref (Just key) >> releaseWrite rwl >> return ())
                  return True
             Nothing -> do
               onCommit (releaseWrite rwl >> return ())
               onAbort (releaseWrite rwl >> return ())
               return False

----------------------

tryAcquireWrite :: RWLock -> IO Bool
tryAcquireWrite rwl = do
     (_,wn1)<-checkLock rwl
     mok <- timeout 10000 (acquireWrite rwl)
     case mok of
              Nothing -> do 
                        (_,wn2)<-checkLock rwl
                        if wn2>wn1 then releaseWrite rwl >> return False else return False
              Just () -> return True

tryAcquireRead :: RWLock -> IO Bool
tryAcquireRead rwl = do
     (rn1,_)<-checkLock rwl
     mok <- timeout 10000 (acquireRead rwl)
     case mok of
              Nothing -> do 
                        (rn2,_)<-checkLock rwl
                        if rn2>rn1 then releaseRead rwl >> return False else return False
              Just () -> return True
