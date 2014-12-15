module Main (main, run) where
import HSbf.VM
import HSbf.Parser
import System.Environment
import System.IO

run p = mtarrband >>= \b -> foldarr (optimize $ optimize $ parse p) b

main = do
	args <- getArgs
	hSetBuffering stdin NoBuffering
	hSetBuffering stdout NoBuffering
	program <- readFile (head args)
	run program
