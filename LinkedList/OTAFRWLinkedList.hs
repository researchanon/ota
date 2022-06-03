module OTAFRWLinkedList where

import NewCASList
import GHC.Conc.Sync
import Control.Monad.IO.Class (MonadIO(..))
import Control.Concurrent.FairRWLock
import Data.IORef
import System.Timeout
import OTA



data ListHandle a = TBL RWLock (NewCASList.ListHandle a)

newList :: IO (OTAFRWLinkedList.ListHandle a)
newList = do
  --alock <- newALock size
  list <- NewCASList.newList
  rwl <- new
  return (TBL rwl list)

fromList :: NewCASList.ListHandle a -> IO (OTAFRWLinkedList.ListHandle a)
fromList list = do
              rwl <- new
              return (TBL rwl list)


add :: Eq a => OTAFRWLinkedList.ListHandle a -> a -> STM ()
add (TBL rwl list) key = runOT $ do
     ok <- liftIO $ tryAcquireWrite rwl
     if not ok then abort
               else do
                 ioref<-liftIO $ NewCASList.addToTail list key 
                 onCommit unlock
                 onAbort (undo ioref) 
  where  
    undo ioref = do
           writeIORef ioref Nothing
           NewCASList.delete list Nothing
           releaseWrite rwl
           return ()
    unlock = releaseWrite rwl >> return ()

contains :: Eq a => OTAFRWLinkedList.ListHandle a -> a -> STM Bool
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
 
 
remove :: Eq a => OTAFRWLinkedList.ListHandle a -> a -> STM Bool
remove (TBL rwl list) key = runOT $ do
     ok <- liftIO $ tryAcquireWrite rwl
     if not ok then abort
               else do
         mnode <- liftIO $ NewCASList.find list key
         case mnode of
             Just ioref -> do 
                  liftIO $ writeIORef ioref Nothing
                  onCommit (NewCASList.delete list (Nothing) >> releaseWrite rwl >> return ())
                  onAbort (writeIORef ioref (Just key) >> releaseWrite rwl >> return ())
                  return True
             Nothing -> do
               onCommit (releaseWrite rwl >> return ())
               onAbort (releaseWrite rwl >> return ())
               return False

----------------------

tryAcquireWrite :: RWLock -> IO Bool
tryAcquireWrite rwl = do
            acquireWrite rwl
            return True



tryAcquireRead :: RWLock -> IO Bool
tryAcquireRead rwl = do
          acquireRead rwl
          return True
