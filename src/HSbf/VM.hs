module HSbf.VM (foldarr, mtarrband) where
import HSbf.Types

import Data.Char
import System.Environment
import System.IO
import Data.Word
import Data.Array.IO 
import System.Exit

mtarrband :: IO ArrBand
mtarrband = newArray (0,30000) 0 >>= \a -> return (a,0)

foldarr :: [Command] -> ArrBand -> IO ArrBand
foldarr [] b = return b
foldarr (Inc:cs) b = incarr b >>= \b -> foldarr cs b
foldarr (Dec:cs) b = decarr b >>= \b -> foldarr cs b
foldarr (Next:cs) b = nextarr b >>= \b -> foldarr cs b
foldarr (Prev:cs) b = prevarr b >>= \b -> foldarr cs b
foldarr (Readc:cs) b = readcarr b >>= \b -> foldarr cs b
foldarr (Writec:cs) b = writecarr b >>= \b -> foldarr cs b
foldarr (Rep x:cs) b = reparr x b >>= \b -> foldarr cs b
foldarr (Loop q x:cs) b = looparr (fromIntegral 1) x b >>= \b -> foldarr cs b
foldarr (Set x:cs) b = setarr (fromIntegral x) b >>= \b -> foldarr cs b
foldarr (Add n:cs) b = addarr (fromIntegral n) b >>= \b -> foldarr cs b
foldarr (AddMulTo l n m:cs) b = addmultoarr (fromIntegral l) n (fromIntegral m) b >>= \b -> foldarr cs b
foldarr (AddTo n m:cs) b = addtoarr n (fromIntegral m) b >>= \b -> foldarr cs b
foldarr (Shift n:cs) b = shiftarr n b >>= \b -> foldarr cs b
foldarr (Times n Readc:cs) b = readmcarr n b >>= \b -> foldarr cs b
foldarr (Times n Writec:cs) b = writemcarr n b >>= \b -> foldarr cs b
foldarr (x:cs) b = print x >> exitFailure

incarr :: ArrBand -> IO ArrBand
incarr (a,i) = readArray a i >>= \n -> writeArray a i (n+1) >> return (a,i)
decarr (a,i) = readArray a i >>= \n -> writeArray a i (n-1) >> return (a,i)
nextarr (a,i) = return (a,i+1)
prevarr (a,i) = return (a,i-1)
readcarr (a,i) = getChar >>= \c -> writeArray a i (fromIntegral $ ord c) >> return (a,i)
writecarr (a,i) = readArray a i >>= \n -> putChar (chr $ fromIntegral n) >> return (a,i)

arr2list :: IOUArray Int Int -> IO [Int]
arr2list a = arr2list' a 0
arr2list' a i 
	| i < 30000 = readArray a i >>= \c -> arr2list' a (i+1) >>= \cs -> return (c:cs)
	| otherwise = return []

usedarr a = reverse $ dropWhile (\x -> x == 0)$ reverse a

printarr a = arr2list a >>= \l -> print $usedarr l

fi = fromIntegral

looparr p c (a,i) = do
	n <- readArray a i
	if n == 0
		then return (a,i)
		else do 
			foldarr c (a,i)
			m <- readArray a i
			if m == 0
				then return (a,i)
				else do
					if n == m
						then reparr c (a, i) >>= \b -> return b
						else do 
							(a',i') <- foldlooparr ((fi p)*(fi n) `div` ((fi n)-(fi m))-1) c (a,i)
							setarr 0 (a,i)
							return (a,i)

foldlooparr ::Int ->  [Command] -> ArrBand -> IO ArrBand
foldlooparr i [] b = return b
foldlooparr i (Loop n x:cs) b = looparr (fromIntegral i) x b >>= \b -> foldlooparr i cs b
foldlooparr i (Set x:cs) b = setarr (fromIntegral x) b >>= \b -> foldlooparr i cs b
foldlooparr i (Add n:cs) b = addarr (fromIntegral (i*n)) b >>= \b -> foldlooparr i cs b
foldlooparr i (AddMulTo l n m:cs) b = addmultoarr (fromIntegral l) n (fromIntegral (m*i)) b >>= \b -> foldlooparr i cs b
foldlooparr i (AddTo n m:cs) b = addtoarr n (fromIntegral (i*m)) b >>= \b -> foldlooparr i cs b
foldlooparr i (Shift n:cs) b = shiftarr n b >>= \b -> foldlooparr i cs b
foldlooparr i (x:xs) b = print x >> exitFailure


reparr c (a,i) = readArray a i >>= \n -> if n == 0 then return (a,i) else foldarr c (a,i) >>= \b -> reparr c b

addtoarr :: Int -> Int -> ArrBand -> IO ArrBand
addtoarr n m (a,i) = readArray a (i+n) >>= \c -> writeArray a (i+n) (m+c) >> return (a,i)

addmultoarr :: Int -> Int -> Int -> ArrBand -> IO ArrBand
addmultoarr l n m (a,i) = do 
	c <- readArray a i 
	d <- readArray a (i+n) 
	writeArray a (i+n) (d+((c*m) `div` (-l)))
	return (a,i)

addarr :: Int -> ArrBand -> IO ArrBand
addarr n (a,i) = readArray a i >>= \c -> writeArray a i (n+c) >> return (a,i)
setarr :: Int -> ArrBand -> IO ArrBand
setarr n (a,i) = writeArray a i n >> return (a,i)
shiftarr n (a,i) = return (a, i+n)
readmcarr n (a,i) = getChars >> getChar >>= \c -> writeArray a i (fromIntegral $ ord c) >> return (a,i)
	where getChars = foldl (\a f -> a >> f) getChar (take (n-2) $ repeat getChar)
writemcarr n (a,i) = readArray a i >>= \c -> putStr (take n $ repeat (chr $ fromIntegral c)) >> return (a,i)
