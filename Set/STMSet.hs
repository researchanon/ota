module STMSet where

import GHC.Conc.Sync

data Node a = Node { val :: a, next :: TVar (Node a)}
            | Null


data Set a = Set { headList :: TVar (Node a)}

printSet :: Show a => Set a -> IO ()
printSet set = printSet2 (headList set)

printSet2 :: Show a => TVar (Node a) -> IO ()
printSet2 set = do
  hd <- atomically (readTVar set)
  case hd of
    Null -> putStrLn "Null"
    Node x v -> putStr (show x) >> putStr "->" >> printSet2 v

newAtomic x = atomically (newTVar x)

newSet :: IO (Set a)
newSet  =
   do null <- newAtomic Null
      return (Set {headList = null })



add :: (Eq a,Ord a) => Set a -> a -> STM Bool
add set v = add2 (headList set) v

add2 :: (Eq a, Ord a) => TVar (Node a) -> a -> STM Bool
add2 tvar v = do
  hd <- readTVar tvar
  case hd of
    Null -> do
      next <- newTVar Null
      writeTVar tvar (Node v next)
      return True
    Node x next -> do
      if x == v
        then return False
        else do
          if v>x
            then add2 next v
            else do
              newNext <- newTVar (Node x next)
              writeTVar tvar (Node v newNext)
              return True

contains :: (Eq a, Ord a) => Set a -> a -> STM Bool
contains set a = contains2 (headList set) a

contains2 :: (Eq a, Ord a) => TVar (Node a) -> a -> STM Bool
contains2 tvar v = do
  hd <- readTVar tvar
  case hd of
    Null -> return False
    Node x next -> do
      if x == v
        then return True
        else do
          if v>x
            then contains2 next v
            else return False

remove :: (Eq a, Ord a) => Set a -> a -> STM Bool
remove set a = remove2 (headList set) a

remove2 :: (Eq a, Ord a) => TVar (Node a) -> a -> STM Bool
remove2 tvar v = do
  hd <- readTVar tvar
  case hd of
    Null -> return False
    Node x next -> do
      if x == v
        then do
          node <- readTVar next
          writeTVar tvar node
          return True
        else do
          if v>x
            then remove2 next v
            else return False
