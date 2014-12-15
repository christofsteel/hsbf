module HSbf.Types (
	ArrBand,
	Command(Inc, Dec, Next, Prev, Readc, Writec, Times, Rep, Set, Add, AddMulTo, AddTo, Shift, Loop)
	)
where 
import Data.Array.IO 
type ArrBand = (IOUArray Int Int, Int)

data Command = Inc | Dec | Next | Prev | Readc | Writec | Times Int Command | Rep [Command] | RepR [Command] | Set Int | Add Int | AddMulTo Int Int Int | AddTo Int Int | Shift Int | Map [Int] Int | Loop Int [Command] deriving (Show, Eq)

