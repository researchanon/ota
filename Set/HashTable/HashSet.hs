module HashTable.HashSet (
	HTable
	,fhash
	,newHash
	,insert
	,contains
	,delete
	) where

import Data.Array
import Data.IORef
import Control.Monad
import Control.Concurrent
import Control.Concurrent.MVar
import Data.Atomics


type Buckets k v = Array Int (MVar [(k,v)])
type Locks k v = IORef (Buckets k v)

data ThId = ThId ThreadId | Null
	deriving(Eq, Show)

data HTable k v = TH
	{
	buckets	 :: Locks k v,			-- Na implementação com lock fino, a tabela é os locks
	n_elemens :: IORef Int, 			-- armazena o número de inserções na tabela hash
	len_tab	 :: IORef Int, 			-- armazena o tamanho da tabela hash
	onGrow	 :: IORef ThId, 		-- usado como flag para sinalizar que está em crescimento
	fHash	 :: (k -> Int)
	}

fhash :: Int -> Int
fhash a = a


newHash :: Int -> (k -> Int) -> IO (HTable k v)
newHash initSize fHash = do
	slots <- replicateM initSize (newMVar [])
	buckets <- newIORef =<< let arrayHash' = listArray (0,(initSize-1)) slots in arrayHash' `seq` return arrayHash'
	n_elemens <- newIORef 0
	len_tab <- newIORef initSize
	onGrow <- newIORef Null
	return (TH buckets n_elemens len_tab onGrow fHash)

--  --------------------- função de inserção do valor ------------------------------

insert :: Eq k => Eq v => HTable k v -> k -> v -> IO Bool
insert table@(TH buckets n_elemens len_tab onGrow fHash) key value = do
	tId <- myThreadId
	(slot,size,ptr) <- lockAcquire buckets len_tab onGrow tId fHash key	-- adquire o lock especifico a posição da tabela
	nInsert <- readIORef n_elemens
	case lookup key slot of
		Just a -> lockRelease ptr slot >> return False
		Nothing -> if (threshold nInsert size) then do
						newSlot <- let l = (key,value):slot in l `seq` return l
						lockRelease ptr newSlot 								-- libera o lock com o slot modificado
						incHashInsert n_elemens
						return True
						else do
							lockRelease ptr slot
							resize buckets size len_tab onGrow tId fHash
							insert table key value
							return True

-- -------------------- função de consulta do valor ------------------------------
contains :: Eq k => Eq v => HTable k v -> k -> IO (Maybe v)
contains table@(TH buckets n_elemens len_tab onGrow fHash) key = do
	tId <- myThreadId 														-- le o valor do id da Thread onde a função foi chamada
	(slot,size,ptr) <- lockAcquire buckets len_tab onGrow tId fHash key 	-- função de aquisição do lock, retorna o slot atual, o tamanho da tabela e o lock em si
	lockRelease ptr slot
	return $ lookup key slot
-- -------------------------------------------------------------------------------

-- ------------------- função de remoção do valor --------------------------------
delete :: Eq k => Eq v => HTable k v -> k -> IO Bool
delete table@(TH buckets n_elemens len_tab onGrow fHash) key = do
	tId <- myThreadId
	(slot,size,ptr) <- lockAcquire buckets len_tab onGrow tId fHash key
	case lookup key slot of
		Just a -> do
			nl <- let l = filter (\x -> key /= fst x) slot in l `seq` return l
			lockRelease ptr nl
			decHashInsert n_elemens
			return True
		Nothing -> lockRelease ptr slot >> return False
-- -------------------------------------------------------------------------------

---- ---------------- Funções auxiliares -------------------------------
atomCAS :: Eq a => IORef a -> a -> a -> IO Bool
atomCAS ptr old new =
   atomicModifyIORefCAS ptr (\ cur -> if cur == old
                                   then (new, True)
                                   else (cur, False))

incHashInsert :: IORef Int -> IO Bool
incHashInsert var = do
	old <- readIORef var
	new <- let n = old+1 in n `seq` return n
	ok <- atomCAS var old new
	if ok then return True else incHashInsert var

decHashInsert :: IORef Int -> IO Bool
decHashInsert var = do
	old <- readIORef var
	new <- let n = old-1 in n `seq` return n
	ok <- atomCAS var old new
	if ok then return True else decHashInsert var

lockAcquire :: Eq k => Locks k v -> IORef Int -> IORef ThId -> ThreadId -> (k -> Int) -> k -> IO ([(k,v)], Int, MVar [(k,v)])
lockAcquire locks len_tab flag tId fhash key = do
	checkOnGrow flag tId									 		-- checa se a tabela não esta em um processo de crescimento
 	size <- readIORef len_tab 								 		-- le o tamanho da tabela
	oldBuckets <- readIORef locks 							 		-- le o array de MVars
	size2 <- readIORef len_tab										-- lê o tamanho da tabela pela segunda vez para ver se não teve alteração
	if (size == size2) then do
		l <- tryTakeMVar $ oldBuckets ! hashfun (fhash key) size 	-- tenta adquirir o conteudo do lock bloqueando a posição
		case l of
			Just slot -> do
				size3 <- readIORef len_tab
				f <- readIORef flag
				if (f == Null || size == size3) then do
					return (slot,size,oldBuckets ! hashfun (fhash key) size)
					else do
						lockRelease (oldBuckets ! (hashfun (fhash key) size)) slot
						lockAcquire locks len_tab flag tId fhash key
			Nothing -> lockAcquire locks len_tab flag tId fhash key
		else lockAcquire locks len_tab flag tId fhash key

{-# INLINABLE checkOnGrow #-}
checkOnGrow :: IORef ThId -> ThreadId -> IO Bool
checkOnGrow flag tId = do
	t <- readIORef flag
	if (t == Null) then return True else checkOnGrow flag tId

lockRelease :: MVar [(k,v)] -> [(k,v)] -> IO ()
lockRelease lock slot = putMVar lock slot
-- -------------------------------------------------------------------------------------------

hashfun :: Int -> Int -> Int
hashfun val tam_tab = mod val tam_tab

threshold :: Int -> Int -> Bool
threshold n_elemens tamTab = fromIntegral n_elemens * 0.75 <= fromIntegral tamTab

copy_list :: Eq k => Eq v => [(k,v)] -> Buckets k v -> Int -> (k -> Int) -> IO Bool
copy_list [] array1 tam_array  fHash = return True
copy_list ((k,v):xs) array1 tam_array fHash = do
	list <- takeMVar $ array1 ! hashfun (fHash k) tam_array
	newList <- let nl = (k,v):list in nl `seq` return nl
	putMVar (array1 ! hashfun (fHash k) tam_array) newList
	copy_list xs array1 tam_array fHash

resizeArray :: Eq k => Eq v => Buckets k v -> Buckets k v -> Int -> Int -> (k -> Int) -> IO (Buckets k v)
resizeArray array1 array2 0 new_tam fHash = return array2
resizeArray array1 array2 old_tam new_tam fHash = do
	lista <- takeMVar $ array1 ! (old_tam-1)
	if (lista /= []) then do
			copy_list lista array2 new_tam fHash
			putMVar (array1 ! (old_tam-1)) lista
			resizeArray array1 array2 (old_tam-1) new_tam fHash
		else do
			putMVar (array1 ! (old_tam-1)) lista
			resizeArray array1 array2 (old_tam-1) new_tam fHash

-- ---------------Implementação do atomCAS ----------------------

resize :: Eq k => Eq v => IORef (Buckets k v) -> Int -> IORef Int -> IORef ThId -> ThreadId -> (k -> Int) -> IO Bool
resize buckets oldSize len_tab flag tId fHash = do
	f <- readIORef flag
	if (f==Null) then do
		old_slots <- readIORef buckets
		ok <- atomCAS flag f (ThId tId)
		if ok then do
			size <- readIORef len_tab
			if(oldSize == size) then do
				slots <- replicateM (size*2) (newMVar [])
				new_slots <- let arrayHash' = listArray (0,(size*2)-1) slots in arrayHash' `seq` return arrayHash'
				newHash <- resizeArray old_slots new_slots oldSize (oldSize*2) fHash
				writeIORef buckets newHash
				writeIORef len_tab (oldSize*2)
				writeIORef flag Null
				return True	else writeIORef flag Null >> return False
		else return False
	else return False
