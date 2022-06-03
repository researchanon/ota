
  module STMList where

  import Data.IORef
  import Control.Concurrent
  import Control.Concurrent.STM


  data List a = Node { val :: a, next :: TVar (List a)}   
            | Null
            | Head {next :: TVar (List a) }


  data ListHandle a = ListHandle { headList :: TVar (TVar (List a)),
                             tailList :: TVar (TVar (List a)) }




  newAtomic x = atomically (newTVar x)

  readAtomic x = atomically (readTVar x)

  writeAtomic ptr x = atomically (writeTVar ptr x)

  -- we create a new list
  newList :: IO (ListHandle a)
  newList = 
    do null <- newAtomic Null
       hd <- newAtomic (Head {next = null })
       hdPtr <- newAtomic hd
       tailPtr <- newAtomic null
       return (ListHandle {headList = hdPtr, tailList = tailPtr})


  find ::  Eq a => ListHandle a -> a -> IO Bool
  find (ListHandle {headList = ptrPtr})  i = 
   atomically (
           do ptr <- readTVar ptrPtr
              head <- readTVar ptr
              case head of 
                  Head {next = startptr} -> find2 startptr i)
    where
     find2 :: Eq a => TVar (List a) -> a -> STM Bool
     find2 curNodePtr i = do
     { curNode <- readTVar curNodePtr
     ; case curNode of
         Null -> return False  -- we've reached the end of the list
                               -- element not found
         Node {val = curval, next = curnext} ->
           if (curval == i) then return True -- element found
           else find2 curnext i              -- keep searching
     }


  -- we add a new node, by overwriting the null tail node
  -- we only need to adjust tailList but not headList because
  -- of the static Head
  -- we return the location of the newly added node
  addToTail :: ListHandle a -> a -> IO (TVar (List a))
  addToTail (ListHandle {tailList = tailPtrPtr}) x =
    do tPtr <- atomically (
                 do null <- newTVar Null
                    tailPtr <- readTVar tailPtrPtr
                    writeTVar tailPtr (Node {val = x, next = null})
                    writeTVar tailPtrPtr null
                    return tailPtr
               )
       return tPtr


  delete :: Eq a => ListHandle a -> a -> IO Bool
  delete (ListHandle {headList = ptrPtr})  i = 
   atomically (
           do startptr <- readTVar ptrPtr
              delete2 startptr i)
    where
     delete2 :: Eq a => TVar (List a) -> a -> STM Bool
     delete2 prevPtr i =
      do
     { prevNode <- readTVar prevPtr
     ; let curNodePtr = next prevNode --  head/node have both next 
     ; curNode <- readTVar curNodePtr
     ; case curNode of
         Null -> return False  -- we've reached the end of the list
                               -- element not found
         Node {val = curval, next = nextNode} ->
           if (curval /= i) 
            then delete2 curNodePtr i         -- keep searching
            else 
                -- delete element (ie delink node)
                do { case prevNode of
                       Head {} -> do writeTVar prevPtr (Head {next = nextNode})
                                     return True
                       Node {} -> do writeTVar prevPtr
                                        (Node {val = val prevNode, next = nextNode})
                                     return True
                   }
      }



