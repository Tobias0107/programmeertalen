import System.Environment
import Data.List
import Distribution.Simple.Program.HcPkg (list)

type Row = Int
type Column = Int
type Value = Int
type Grid = [[Value]] -- Only used to read/write from/to a file.
type Sudoku = (Row,Column) -> Value
type Constraint = (Row, Column, [Value])
type Node = (Sudoku, [Constraint])

positions :: [Int]
positions = [1..9]

values :: [Value]
values = [1..9]

blocks :: [[Int]]
blocks = [[1..3],[4..6],[7..9]]

centerOfBlocks :: [Int]
centerOfBlocks = [2, 5, 8]

sud2grid :: Sudoku -> Grid
sud2grid s = [[s (r, c) | c <- positions] | r <- positions]

grid2sud :: Grid -> Sudoku
grid2sud gr = \(r, c) -> pos gr (r, c)
  where pos :: [[a]] -> (Row,Column) -> a
        pos gr (r, c) = (gr !! (r - 1)) !! (c - 1)

-- Extends a sudoku with a value at (row, column).
extend :: Sudoku -> (Row, Column, Value) -> Sudoku
extend sud (r, c, v) (i, j) = if r == i && c == j then v else sud (i, j)

-- Read a file-sudoku with a Grid like format into a Sudoku.
readSudoku :: String -> IO Sudoku
readSudoku filename =
    do stringGrid <- readFile filename
       return $ (grid2sud . splitStringIntoGrid) stringGrid
       where splitStringIntoGrid = map (map readint . words) . lines
             readint x = read x :: Int

{- Prints a Sudoku to the terminal by transforming it to a grid first.
   Do not modify this, or your tests will fail.
-}
printSudoku :: Sudoku -> IO ()
printSudoku = putStr . unlines . map (unwords . map show) . sud2grid

-- Helper to parse command-line arguments.
getSudokuName :: [String] -> String
getSudokuName [] = error "Filename of sudoku as first argument."
getSudokuName (x:_) = x

main :: IO ()
main =
    do args <- getArgs
       sud <- (readSudoku . getSudokuName) args
       print$freeInSubgrid sud (7,7)
       printSudoku sud

freeInRow :: Sudoku -> Row -> [Value]
freeInRow sudoku row = [1 .. 9] \\ [sudoku (row, x) | x <- [1 .. 9],
                                    sudoku (row, x) /= 0]

freeInColumn :: Sudoku -> Column -> [Value]
freeInColumn sudoku col = [1 .. 9] \\ [sudoku (x, col) | x <- [1 .. 9],
                                    sudoku (x, col) /= 0]

{- The subgrid should have all values from 1-9 once in the subgrid.
   This function returns a list of the values that are not in the subgrid yet.
   The subgrid tested is the subgrid that contains the box given by the given
   row and collumn. (The subgrid of the given Sudoku)
-}
freeInSubgrid :: Sudoku -> (Row, Column) -> [Value]
freeInSubgrid sudoku (row, col) = [1 .. 9] \\ [sudoku (x, y) |
   x <- [startrow .. endrow], y <- [startcol .. endcol], sudoku (x, y) /= 0]
      where startrow = sum (
               map (\x -> if row `elem` x then head x else 0) blocks)
            endrow = startrow + 2
            startcol = sum (
               map (\x -> if col `elem` x then head x else 0) blocks)
            endcol = startcol + 2
