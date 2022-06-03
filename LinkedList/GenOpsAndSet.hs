module GenOpsAndSet where

import System.Random
import System.Random.Shuffle
import System.IO
import System.Environment

-- TestSTMSet 3000 2000 5000 0 8

data Op = Contains Int | Remove Int | Add Int
           deriving(Eq,Show,Read)



main1 = do
  args <- getArgs
  let range = read (args!!0)
  let sizeSet = read (args!!1)
  let sizeOps = read (args!!2)
  let readsP  = read (args!!3)
  let readsP2 = 100 - readsP
  let opsFileRead =  (args!!4)
  let opsFileWrite = (args!!5)
  let setfile =  (args!!6)
  genOpsFile sizeOps readsP range opsFileRead
  genOpsFile sizeOps readsP2 range opsFileWrite
  genSetFile sizeSet range setfile
  
  

genOpsFile :: Int -> Int -> Int -> FilePath -> IO ()
genOpsFile total readsP range file = do
                          ops <- genListOps total readsP range
                          writeFile file (show ops)

genSetFile :: Int -> Int -> FilePath -> IO ()
genSetFile size range file = do
                           set <- genSet range size []
                           writeFile file (show set)

genListOps :: Int -> Int -> Int -> IO [Op]
genListOps total readsP range = do
                         let ncontains = div (total*readsP) 100
                         let nwrites = total - ncontains
                         let nremoves = div nwrites 2
                         let nadds = nwrites - nremoves
                         assert (total == ncontains + nremoves + nadds) (do
                                                                           lcontains <- genOps Contains range ncontains
                                                                           ladds <- genOps Add range nadds
                                                                           lremoves <- genOps Remove range nremoves
                                                                           let lops = lcontains ++ ladds ++ lremoves
                                                                           rng <- newStdGen
                                                                           let lopsRandom = shuffle' lops (length lops) rng
                                                                           return lopsRandom)

readOpsFile :: String -> IO [Op]
readOpsFile file = do
            str <- readFile file
            return (read str)

readSetFile :: String -> IO [Int]
readSetFile file = do
            str <- readFile file
            return (read str)


genSet ::  Int -> Int -> [Int] -> IO [Int]
genSet range 0 set    = return set
genSet range size set = do
  v <-randomRIO (1, range)
  let found = findSet v set
  if found then genSet range size set else genSet range (size - 1) (v:set)
  

findSet :: Int -> [Int] -> Bool
findSet v [] = False
findSet v (x:xs)
   | v == x = True
   | otherwise = findSet v xs



assert :: Bool -> a -> a
assert False x = error "assertion failed!"
assert _     x = x

genOps :: (Int -> Op) -> Int -> Int -> IO [Op]
genOps const range 0 = return []
genOps const range n = do
  v <-randomRIO (1, range)
  list <- genOps const range (n-1)
  return (const v:list)
