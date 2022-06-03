module OTASet where

import CASList
import AbstractLock
import GHC.Conc.Sync
import Control.Monad.IO.Class (MonadIO(..))
import OTA



data Set a = TBS (ALock a) (CASList.ListHandle a)

newSet :: Int -> (a->Int) -> IO (Set a)
newSet size f = do
  alock <- newALock size f
  list <- CASList.newList
  return (TBS alock list)


add :: Eq a => Set a -> a -> STM Bool
add (TBS alock list) key =  runOT $ do
  ok <- liftIO $ lock alock key
  if ok
     then do
        onCommit unlock
        found <- liftIO $ CASList.find list key
        if found
           then do
             onAbort unlock
             return False
           else do
             liftIO $ CASList.addToTail list key
             onAbort (undo >> unlock)
             return True
     else abort
  where
    undo = CASList.delete list key >> return ()
    unlock = AbstractLock.unlock alock key >> return ()


contains :: Eq a =>  Set a -> a -> STM Bool
contains (TBS alock list) key =  runOT $ do
  ok <- liftIO $ lock alock key
  if ok
    then do
      onCommit unlock
      onAbort unlock
      liftIO $ CASList.find list key
    else abort
  where
    unlock = AbstractLock.unlock alock key >> return ()

remove :: Eq a =>  Set a -> a -> STM Bool
remove (TBS alock list) key = runOT $ do
  ok <- liftIO $ lock alock key
  if ok
    then do
      onCommit unlock
      onAbort unlock
      removed <- liftIO $ CASList.delete list key
      if removed
        then onAbort undo >> return removed
        else return removed
    else abort
  where
    undo = CASList.addToTail list key
    unlock = AbstractLock.unlock alock key >> return ()
