module NewCASList where

import GHC.IO
import Data.IORef
import Data.Atomics
import Control.Concurrent
import Control.Concurrent.Chan
import System.Environment
import Data.Time

data List a = Node { val :: IORef (Maybe a)
                   , next :: IORef (List a) }
            | DelNode { next :: IORef (List a) }
            | Null
            | Head { next :: IORef (List a) } deriving Eq

data ListHandle a = ListHandle { headList :: IORef (IORef (List a)),
                             tailList :: IORef (IORef (List a)) }


-- we assume a static head pointer, pointing to the first node which must be Head
-- the deleted field of Head is always False, it's only there to make some of the code
-- more uniform
-- tail points to the last node which must be Null


type Iterator a = IORef (IORef (List a))


-------------------------------------------
-- auxilliary functions



while b cmd = if b then do {cmd; while b cmd}
              else return ()

repeatUntil cmd = do { b <- cmd; if b then return ()
                                  else repeatUntil cmd }

atomCAS :: Eq a => IORef a -> a -> a -> IO Bool
atomCAS ptr old new =
   atomicModifyIORefCAS ptr (\ cur -> if cur == old
                                   then (new, True)
                                   else (cur, False))

atomicWrite :: IORef a -> a -> IO ()
atomicWrite ptr x =
   atomicModifyIORefCAS ptr (\ _ -> (x,()))


----------------------------------------------
-- functions operating on lists


-- we create a new list
newList :: IO (ListHandle a)
newList =
   do null <- newIORef Null
      hd <- newIORef (Head {next = null })
      hdPtr <- newIORef hd
      tailPtr <- newIORef null
      return (ListHandle {headList = hdPtr, tailList = tailPtr})


-- we add a new node, by overwriting the null tail node
-- we only need to adjust tailList but not headList because
-- of the static Head
-- we return the location of the newly added node
addToTail :: Eq a => ListHandle a -> a -> IO (IORef (Maybe a))
addToTail (ListHandle {tailList = tailPtrPtr}) x =
   do null <- newIORef Null
      newValue <- newIORef (Just x)
      repeatUntil
         (do tailPtr <- readIORef tailPtrPtr
             b <- atomCAS tailPtr Null (Node {val = newValue, next = null})
             return b )
        -- we atomically update the tail
        -- (by spinning on the tailPtr)
      atomicWrite tailPtrPtr null
      return newValue


find :: Eq a => ListHandle a -> a -> IO (Maybe (IORef (Maybe a)))
find (ListHandle { headList = head }) x =
  let go prevPtr =
        do do prevNode <- readIORef prevPtr
              let curPtr = next prevNode -- head/node/delnode have all next
              curNode <- readIORef curPtr
              case curNode of
                Node {val = y, next = nextNode } -> do
                   value <-readIORef y
                   if (Just x == value)
                   then -- node found and alive
                      return (Just y)
                   else go curPtr -- continue
                Null -> return Nothing -- reached end of list
                DelNode {next = nextNode } ->
                         -- atomically delete curNode by setting the next of prevNode to next of curNode
                         -- if this fails we simply move ahead
                        case prevNode of
                          Node {} -> do b <- atomCAS prevPtr prevNode (Node {val = val prevNode,
                                                                             next = nextNode})
                                        if b then go prevPtr
                                         else go curPtr
                          Head {} -> do b <- atomCAS prevPtr prevNode (Head {next = nextNode})
                                        if b then go prevPtr
                                         else go curPtr
                          DelNode {} -> go curPtr    -- if parent deleted simply move ahead
             {-
                correct as well, but a deleted parent deleting a child is (for certain cases) a useless operation
                                     do atomicModifyIORef prevPtr ( \ cur -> (cur{next = nextNode},True))
                                        go prevPtr
              -}

  in do startPtr <- readIORef head
        go startPtr




delete :: Eq a => ListHandle a -> IORef (Maybe a) -> IO Bool
delete (ListHandle { headList = head }) x =
  let go prevPtr =
        do do prevNode <- readIORef prevPtr
              let curPtr = next prevNode -- head/node/delnode have all next
              curNode <- readIORef curPtr
              case curNode of
                Node {val = y, next = nextNode } -> do
                   if (x == y)
                   then -- node found and alive
                      do b <- atomCAS curPtr curNode (DelNode {next = nextNode})
                         if b then return True
                          else go prevPtr -- spin
                   else go curPtr -- continue
                Null -> return False -- reached end of list
                DelNode {next = nextNode } ->
                         -- atomically delete curNode by setting the next of prevNode to next of curNode
                         -- if this fails we simply move ahead
                        case prevNode of
                          Node {} -> do b <- atomCAS prevPtr prevNode (Node {val = val prevNode,
                                                                             next = nextNode})
                                        if b then go prevPtr
                                         else go curPtr
                          Head {} -> do b <- atomCAS prevPtr prevNode (Head {next = nextNode})
                                        if b then go prevPtr
                                         else go curPtr
                          DelNode {} -> go curPtr    -- if parent deleted simply move ahead

  in do startPtr <- readIORef head
        go startPtr


