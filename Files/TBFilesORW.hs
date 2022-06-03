module TBFilesORW where


import GHC.Conc.Sync
import Control.Monad.IO.Class (MonadIO(..))
import GHC.IO (IO(..))
import           Control.Concurrent.ReadWriteLock        
import System.FileLock
import System.IO
import Control.Exception
import OTA

    
data THandle =  THandle Handle


openFileSTM :: FilePath -> IOMode -> STM THandle
openFileSTM path ReadMode = runOT $ do
   mhdl <- liftIO $ tryOpenFile path ReadMode
   case mhdl of 
      Nothing -> abort
      Just hdl -> do
         mlock <- liftIO $ tryAcquireRead_ path
         case mlock of
            Just lock -> do                         
              onAbort (unlockFile lock >> hClose hdl)
              onCommit (unlockFile lock >> hClose hdl)
              return (THandle hdl)
            Nothing -> abort
openFileSTM path ReadWriteMode = runOT $ do
   mhdl <- liftIO $ tryOpenFile path ReadWriteMode
   case mhdl of 
      Nothing -> abort
      Just hdl -> do
         mlock <- liftIO $ tryAcquireWrite_ path
         case mlock of
            Just lock -> do                         
              onAbort  (unlockFile lock >> hClose hdl)
              onCommit (unlockFile lock >> hClose hdl)
              return (THandle hdl)
            Nothing -> abort

readLineSTM :: THandle -> STM String
readLineSTM (THandle hdl) = performIO $ do
    end <- liftIO $ hIsEOF hdl
    if not end then do 
           pos <- liftIO $ hTell hdl
           line <- liftIO $ hGetLine hdl
           addAbortHandler (hSeek hdl AbsoluteSeek pos)
           return line
    else return "" 


writeLineSTM :: THandle -> String -> STM ()
writeLineSTM (THandle hdl) str = runOT $ do
 let lStr =  length str
 pos <- liftIO $ hTell hdl
 oldStr <- liftIO $ tryReadStr hdl lStr
 liftIO $ hSeek hdl AbsoluteSeek pos
 liftIO $ hPutStrLn hdl str
 let lOld = length oldStr
 if (lOld < lStr)
  then do 
   fSize <- liftIO $ hFileSize hdl
   onAbort (do 
    hSeek hdl AbsoluteSeek pos
    hPutStr hdl oldStr
    let oldSize = fSize-(toInteger (lStr-lOld))
    hSetFileSize hdl oldSize
    hSeek hdl AbsoluteSeek pos)
  else onAbort (do 
    hSeek hdl AbsoluteSeek pos
    hPutStr hdl oldStr
    hSeek hdl AbsoluteSeek pos)     
      
      
      
writeLineEndSTM :: THandle -> String -> STM ()
writeLineEndSTM (THandle hdl) str = runOT $ do
   pos <- liftIO $ hTell hdl
   fSize <- liftIO $ hFileSize hdl
   liftIO $ hSeek hdl AbsoluteSeek fSize
   liftIO $ hPutStrLn hdl str
   onAbort (do
        hSetFileSize hdl fSize
        hSeek hdl AbsoluteSeek pos)


isEOFSTM :: THandle -> STM Bool
isEOFSTM (THandle hdl) = runOT $ liftIO $ hIsEOF hdl
                   


hSeekSTM :: THandle -> Integer -> STM ()
hSeekSTM (THandle hdl) npos = performIO $ do
         pos <- liftIO $ hTell hdl
         liftIO $ hSeek hdl AbsoluteSeek npos
         addAbortHandler (hSeek hdl AbsoluteSeek pos)

hTellSTM :: THandle -> STM Integer
hTellSTM (THandle hdl) = performIO $ do
        pos <- liftIO $ hTell hdl
        return pos
-----------------------------------------------------------------------------


tryReadStr :: Handle -> Int -> IO String
tryReadStr hdl 0 = return []
tryReadStr hdl size = do
        end <- hIsEOF hdl
        if not end then do
            c <-hGetChar hdl
            str <- tryReadStr hdl (size-1)
            return (c:str)
        else return []
-------------------------------------------------------------------------------------------------------

tryOpenFile :: FilePath -> IOMode -> IO (Maybe Handle)
tryOpenFile path mode = tryOpen 100 path mode


tryOpen :: Int -> FilePath -> IOMode -> IO (Maybe Handle)
tryOpen 1 path mode    = tryOpenFile_ path mode
tryOpen n path mode    = do
                     mhdl <- tryOpenFile_ path mode
                     case mhdl of
                        Nothing -> tryOpen (n-1) path mode
                        Just hdl -> return (Just hdl)

tryOpenFile_ :: FilePath -> IOMode -> IO (Maybe Handle)
tryOpenFile_ path mode = do 
                        mhdl <- catch (do f<- openFile path mode
                                          return $ Just f)  catchb
                        return mhdl
  where
   catchb :: SomeException -> IO (Maybe Handle)
   catchb e = return Nothing

---------------------------------------------------------------
tryAcquireRead_ :: FilePath -> IO (Maybe FileLock)
tryAcquireRead_ path = tryAcquireR 100 path

tryAcquireR :: Int -> FilePath -> IO (Maybe FileLock)
tryAcquireR 1 path = tryLockFile path Shared
tryAcquireR n path = do
            ok <- tryLockFile path Shared
            if ok == Nothing then tryAcquireR (n-1) path
                             else return ok

tryAcquireWrite_ :: FilePath -> IO (Maybe FileLock)
tryAcquireWrite_ path = tryAcquireW 100 path

tryAcquireW :: Int -> FilePath -> IO (Maybe FileLock)
tryAcquireW 0 path = tryLockFile path Exclusive
tryAcquireW n path = do
           ok <- tryLockFile path Exclusive
           if ok == Nothing then tryAcquireW (n-1) path 
                             else return ok
