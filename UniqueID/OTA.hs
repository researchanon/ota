module OTA (OT,runOT,onCommit,onAbort,atomic,liftIO)where

import GHC.Conc.Sync 
import Control.Monad.IO.Class (MonadIO(..))
import GHC.IO (IO(..))
import Control.Exception
import Data.IORef

instance MonadIO IOSTM where
    liftIO (IO a) = IOSTM a


type OT = IOSTM
runOT = performIO
onCommit = addCommitHandler
onAbort = addAbortHandler


atomic :: STM a -> IO a
atomic stm = do
           ioref <- newIORef (return () :: IO ())
           atomically (catchSTM (nstm ioref >> stm)  (bcatch ioref))

    where
       nstm ioref = do
          performIO $ addAbortHandler (box ioref)
       box :: IORef (IO ()) -> IO ()
       box ioref = do
         io <- readIORef ioref
         io
       bcatch ::  IORef (IO ()) -> SomeException -> STM a
       bcatch ioref e = performIO $ do
                          liftIO $ writeIORef ioref (throwIO e)
                          abort




