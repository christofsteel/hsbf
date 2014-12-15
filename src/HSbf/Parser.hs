module HSbf.Parser (parse, optimize) where

import HSbf.Types

import Data.Char
import System.Environment
import System.IO
import Data.Array.IO
import Data.Word
import System.Exit

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
