import Data.Char
import System.Environment
import System.IO
import Data.Array.IO
import Data.Word
import System.Exit

type ArrBand = (IOUArray Int Int, Int)

runarr cs = mtarrband >>= \b -> foldarr (parse cs) b
runoptarr cs = mtarrband >>= \b -> foldarr (optimize $ optimize $ parse cs) b

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
--foldarr (Loop x:cs) b = looparr x b >>= \b -> foldarr cs b
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

data Command = Inc | Dec | Next | Prev | Readc | Writec | Times Int Command | Rep [Command] | RepR [Command] | Set Int | Add Int | AddMulTo Int Int Int | AddTo Int Int | Shift Int | Map [Int] Int | Loop Int [Command] deriving (Show, Eq)

parse :: String -> [Command]
parse "" = []
parse ('+':xs) = Inc:parse xs
parse ('-':xs) = Dec:parse xs
parse ('>':xs) = Next:parse xs
parse ('<':xs) = Prev:parse xs
parse (',':xs) = Readc:parse xs
parse ('.':xs) = Writec:parse xs
parse ('[':xs) = (Rep inrep):parse outrep
	where (inrep,outrep) = parseRep ([],xs)
parse (x:xs) = parse xs

parseRep :: ([Command], String) -> ([Command], String)
parseRep (cs,(']':xs)) = (reverse cs,xs)
parseRep (cs,('+':xs)) = parseRep ((Inc:cs),xs)
parseRep (cs,('-':xs)) = parseRep ((Dec:cs),xs)
parseRep (cs,('>':xs)) = parseRep ((Next:cs),xs)
parseRep (cs,('<':xs)) = parseRep ((Prev:cs),xs)
parseRep (cs,(',':xs)) = parseRep ((Readc:cs),xs)
parseRep (cs,('.':xs)) = parseRep ((Writec:cs),xs)
parseRep (cs,('[':xs)) = parseRep ((Rep inrep:cs),outrep)
	where (inrep, outrep) = parseRep ([], xs)
parseRep (cs,(x:xs)) = parseRep (cs, xs)

plusMaybe :: Maybe Int -> Maybe Int -> Maybe Int
plusMaybe (Just a) (Just b) = Just (a + b)
plusMaybe _ Nothing = Nothing
plusMaybe Nothing _ = Nothing

shiftnr :: [Command] -> Maybe Int
shiftnr [] = Just 0
shiftnr (Shift n:xs) = Just n `plusMaybe` (shiftnr xs)
shiftnr (Loop _ n:xs) = Nothing
shiftnr (Rep x:xs) = Nothing
shiftnr (Writec:xs) = Nothing
shiftnr (Readc:xs) = Nothing
shiftnr (Times _ _:xs) = Nothing
shiftnr (x:xs) = shiftnr xs

isLoop (Loop n m) = True
isLoop _ = False

isRep (Rep n) = True
isRep _ = False

optimize :: [Command] -> [Command]
optimize [] = []
optimize (Shift 0:cs) = optimize cs
optimize (Prev:cs) = optimize (Shift (-1):cs)
optimize (Shift n:Prev:cs) = optimize (Shift (n-1):cs)
optimize (Next:cs) = optimize (Shift 1:cs)
optimize (Shift n:Next:cs) = optimize (Shift (n+1):cs)
optimize (Shift n:Shift m:cs) = optimize (Shift (n+m):cs)
optimize (Dec:cs) = optimize (Add (-1):cs)
optimize (Add n:Dec:cs) = optimize (Add (n-1):cs)
optimize (Inc:cs) = optimize (Add 1:cs)
optimize (Add n:Inc:cs) = optimize (Add (n+1):cs)
optimize (Add n:Add m:cs) = optimize (Add (n+m):cs)
optimize (Set n:Set m:cs) = optimize ((Set m):cs)
optimize (Rep x:Rep y:cs)
	| x == y = optimize (Rep x:cs)
	| otherwise = Rep (optimize x):(optimize $ (Rep y):cs)
optimize (Rep [Dec]:cs) = Set 0:optimize cs
optimize (Rep [Add (-1)]:cs) = Set 0:optimize cs
optimize (Shift n: Add m: Shift o:cs)
	| o == (-n) = optimize $ AddTo n m:cs
	| otherwise = optimize $ AddTo n m:Shift (n+o):cs
optimize ((Rep [Add l, AddTo n m]):cs) = optimize $ AddMulTo l n m:Set 0:cs
optimize ((Rep (Add l:AddTo n m:xs)):cs) = optimize $ AddMulTo l n m:Rep (Add l:xs):cs
optimize ((Loop _ [Add l, AddTo n m]):cs) = optimize $ AddMulTo l n m:Set 0:cs
optimize (Rep x:cs) 
	| shiftnr (optimize x) == Just 0 = Loop (-1) (optimize $ optimize x):optimize cs
	| otherwise = Rep (optimize $ optimize x):optimize cs
optimize (Times n a:b:cs)
	| a == b && ((a == Writec) || (a == Readc)) = optimize (Times (n+1) a:cs)
	| otherwise = Times n a:optimize (b:cs)
optimize (a:b:cs) 
	| a == b && ((a == Writec) || (a == Readc)) = optimize (Times 2 a:cs)
	| otherwise = a:optimize (b:cs)
optimize (a:[]) = [a]

main = do
	args <- getArgs
	hSetBuffering stdin NoBuffering
	hSetBuffering stdout NoBuffering
	program <- readFile (head args)
	runoptarr program
