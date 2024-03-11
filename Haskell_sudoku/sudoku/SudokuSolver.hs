{-Name: Tobias van den Bosch
  UvaNettID: 15172635
  Short Description: This file reads a given sudoku file and solves it. Given
  the command nrc behind the run command will make the program consider it an
  nrc sudoku and solve it using those rules. When the sudoku can´t be solved
  the program will give back an error.
-}

import System.Environment
import Data.List
import Distribution.Simple.Program.HcPkg (list)

type Row = Int
type Column = Int
type Value = Int
type Grid = [[Value]] -- Only used to read/write from/to a file.
type Sudoku = (Row,Column) -> Value
type Solver = Sudoku -> Sudoku
type Constraint = (Row, Column, [Value])
type Node = (Sudoku, [Constraint])

positions :: [Int]
positions = [1..9]

values :: [Value]
values = [1..9]

blocks :: [[Int]]
blocks = [[1..3],[4..6],[7..9]]

-- Blocks special for nrc sudoku's (the gray areas)
nrcblocks :: [[Int]]
nrcblocks = [[2..4],[6..8]]

centerOfBlocks :: [Int]
centerOfBlocks = [2, 5, 8]

-- start row/colums of the nrc blocks
nrcStartOfBlocks :: [Int]
nrcStartOfBlocks = [2, 6]

sud2grid :: Sudoku -> Grid
sud2grid s = [[s (r, c) | c <- positions] | r <- positions]

grid2sud :: Grid -> Sudoku
grid2sud gr = \(r, c) -> pos gr (r, c)
  where pos :: [[a]] -> (Row,Column) -> a
        pos gr (r, c) = gr !! (r - 1) !! (c - 1)

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

{-This function returns the values that are not yet in the given row in the
  given sudoku-}
freeInRow :: Sudoku -> Row -> [Value]
freeInRow sudoku row = [1 .. 9] \\ [sudoku (row, x) | x <- [1 .. 9],
                                    sudoku (row, x) /= 0]

{-This function returns the values that are not yet in the given column in the
  given sudoku-}
freeInColumn :: Sudoku -> Column -> [Value]
freeInColumn sudoku col = [1 .. 9] \\ [sudoku (x, col) | x <- [1 .. 9],
                                    sudoku (x, col) /= 0]

{-This function returns the values that are not yet in the subgrid that
  contains the given row and column in the given sudoku. -}
freeInSubgrid :: Sudoku -> (Row, Column) -> [Value]
freeInSubgrid sudoku (row, col) = [1 .. 9] \\ [sudoku (x, y) |
   x <- [startrow .. endrow], y <- [startcol .. endcol], sudoku (x, y) /= 0]
      -- For startrow/col loop through the lists of possible subgrids, if the
      -- given element is part of it, give the first element of that list as
      -- start back.
      where startrow = sum (
               map (\x -> if row `elem` x then head x else 0) blocks)
            endrow = startrow + 2
            startcol = sum (
               map (\x -> if col `elem` x then head x else 0) blocks)
            endcol = startcol + 2

{-This function returns a list of all values of numbers from 1-9 that can still
  be put in the given position of the sudoku (following the rules) -}
freeAtPos :: Sudoku -> (Row,Column) -> [Value]
freeAtPos sud (row,col) = if sud (row, col) == 0 then
   freeInSubgrid sud (row, col) `intersect` freeInRow sud row
   `intersect` freeInColumn sud col
   else []

{-This function is a recursive implementation function used by the
  openPositions function. -}
openPositionsRec :: Sudoku -> [(Int, Int)] -> [(Row,Column)] -> [(Row,Column)]
openPositionsRec sud [] [] = if sud (1,1) == 0 then openPositionsRec
   sud [(1,1)] [(1,1)] else openPositionsRec sud [(1,1)] []
openPositionsRec sud [(x,9)] y = if x + 1 < 10 then openPositionsRec
   sud [(x+1,0)] y else y
openPositionsRec sud [(x,y)] z = if sud (x,y+1) == 0 then openPositionsRec
   sud [(x,y+1)] ((x,y+1):z) else openPositionsRec sud [(x,y+1)] z

{-This function gives back a list of tuples of (Row, Column) that records all
  positions in the sudoku that have a zero.
  The openPositionsRec is used for schematically walking through the sudoku
  and recording and giving back all places a zero was found. -}
openPositions :: Sudoku -> [(Row,Column)]
openPositions sud = openPositionsRec sud [] []

{-This function removes all duplicates within a list-}
removeDuplicates :: Eq a => [a] -> [a]
removeDuplicates [] = []
removeDuplicates (x:xs) = x:removeDuplicates (filter (/= x) xs)

{-This function gives back a Boolian. If the given row of the given sudoku is
  valid (meaning solved and following the rules) than a True is given. False
  otherwise.
  Tested by checking if there are no free places or duplicates in the row-}
rowValid :: Sudoku -> Int -> Bool
rowValid sud row
  | freeInRow sud row /= [] = False
  | length (removeDuplicates [sud (row, x) | x <- [1 .. 9]]) /= 9 = False
  | otherwise = True

{-This function gives back a Boolian. If the given column of the given sudoku
  is valid (meaning solved and following the rules) than a True is given. False
  otherwise.
  Tested by checking if there are no free places or duplicates in the column-}
colValid :: Sudoku -> Column -> Bool
colValid sud col
  | freeInColumn sud col /= [] = False
  | length (removeDuplicates [sud (x, col) | x <- [1 .. 9]]) /= 9 = False
  | otherwise = True

{-This function gives back a Boolian. If the given subgrid of the given sudoku
  is valid (meaning solved and following the rules) than a True is given. False
  otherwise. The subgrid is given by giving a tuple containing a row and column
  that are within the tested subgrid.
  Tested by checking if there are no free places or duplicates in the subgrid-}
subgridValid :: Sudoku -> (Row,Column) -> Bool
subgridValid sud (row, col)
  | freeInSubgrid sud (row, col) /= [] = False
  | length (removeDuplicates [sud (x, y) |
      x <- [startrow .. endrow], y <- [startcol .. endcol], sud (x, y) /= 0])
      /= 9 = False
  | otherwise = True
      where startrow = sum (
               map (\x -> if row `elem` x then head x else 0) blocks)
            endrow = startrow + 2
            startcol = sum (
               map (\x -> if col `elem` x then head x else 0) blocks)
            endcol = startcol + 2

{-This function gives back a Boolian. If the given sudoku is solved and follows
  all the rules, then it is consistent, and an True is given back. False
  otherwise.
  If the boolian False is given then normal rules are used to check. If the
  boolian True is given, then nrc rules are used to check.
  Tested by checking if all the rows, columns, subgrids and possibly if the nrc
  extra blocks are valid.-}
consistent :: Bool -> Sudoku -> Bool
consistent bool sud
  | length (filter (rowValid sud) [1..9]) /= 9 = False
  | length (filter (colValid sud) [1..9]) /= 9 = False
  | length (filter (subgridValid sud)
      [(x,y)|x<-centerOfBlocks, y<-centerOfBlocks]) /= 9 = False
  | bool && length (filter (nrcValid sud)
      [(x,y)|x<-nrcStartOfBlocks, y<-nrcStartOfBlocks]) /= 4 = False
  | otherwise = True

{-This function works, but is slow i guess, since it works when there are less
  zero's. But when there are more zero's it will take some time, and at some
  point it takes so much time i don´t wait for it anymore. It is strange since
  I do exactly the same in this check as in subgridValid checks, and they
  should be executed the exact same time, but subgridValid takes a lot less
  time with a lot of zero's.
  This function checks if given a sudoku, row and column if for the row and
  column in the sudoku if the nrc grid that has that row and column is valid.
  The function returns True if valid, False if invalid. -}
nrcValid :: Sudoku -> (Row,Column) -> Bool
nrcValid sud (row, col)
  | length (removeDuplicates [sud (x, y) |
      x <- [startrow .. endrow], y <- [startcol .. endcol], sud (x, y) /= 0])
      /= 9 = False
  | otherwise = True
      where startrow = sum (
               map (\x -> if row `elem` x then head x else 0) nrcblocks)
            endrow = startrow + 2
            startcol = sum (
               map (\x -> if col `elem` x then head x else 0) nrcblocks)
            endcol = startcol + 2

printNode :: Node -> IO()
printNode = printSudoku . fst

{-This function is an helper recursive function for the constraintsfunction-}
constraintsRec :: Sudoku -> [(Int, Int)] -> [Constraint] -> [Constraint]
constraintsRec sud [] [] = if sud (1,1) == 0 then
   constraintsRec sud [(1,1)] [(1, 1, freeAtPos sud (1,1))]
   else constraintsRec sud [(1,1)] []
constraintsRec sud [(x,9)] y = if x + 1 < 10 then constraintsRec
   sud [(x+1,0)] y else y
constraintsRec sud [(x,y)] z = if sud (x,y+1) == 0 then
   constraintsRec sud [(x,y+1)] ((x, y+1, freeAtPos sud (x,y+1)):z)
   else constraintsRec sud [(x,y+1)] z

{-This function determines how to sort constraints (by length)-}
sortConstraints :: (Row, Column, [Value]) -> (Row, Column, [Value]) -> Ordering
sortConstraints (row, col, list) (row2, col2, list2)
   | length list > length list2 = GT
   | otherwise = LT

{-This function, given an sudoku, will return an sorted list of all constraints
  of the given sudoku. This constraint is, given an empty place in the sudoku
  this constraint will contain a tuple of the row, column and an list of all
  possible values that can be inserted into that place that do not override the
  rules.
  This function recursively walks through the sudoku and will record all
  constraints. At the end all the constraints are sorted by length using the
  sortby sortConstraints functions-}
constraints :: Sudoku -> [Constraint]
constraints sud = sortBy sortConstraints (constraintsRec sud [] [])

{-This function solves the given sudoku and returns it. The boolian that is
  given with the to solve sudoku will determine what rules are used for
  solving. False is normal rules, True is nrc rules.
  -}
sudokuSolve :: Sudoku -> Bool -> Sudoku
sudokuSolve sud bool
  | null (constraints sud) = sud
  | consistent bool sud = sud
  | otherwise = head (if null solution then [sud] else solution)
      where
         (row, col, list) = head(constraints sud)
         solution = filter (consistent bool) (map (\x->
            sudokuSolve (extend sud (row, col, x)) bool) list)

{-This function solves the given sudoku using normal rules. If there is no
  solution then an error is given back-}
normalSolver :: Sudoku -> Sudoku
normalSolver sud = if consistent False s  then s else error "no_solution"
   where
      s = sudokuSolve sud False

{-This function solves the given sudoku using nrc rules. If there is no
  solution then an error is given back-}
nrcSolver :: Sudoku -> Sudoku
nrcSolver sud = if consistent True s  then s else error "no_solution"
   where
      s = sudokuSolve sud True

{-This function, when given the args will give back the right solver using the
  specified rules in args.-}
getSolver :: [String] -> Solver
getSolver (_:"nrc":_) = nrcSolver
getSolver _ = normalSolver

{-This function, will solve the given sudoku using normal rules, or nrc rules
  when specefied. If there is no solution an error is given back.
  If the sudoku is succesfully solved then the solution is printed to stdout.-}
main :: IO ()
main =
    do args <- getArgs
       sud <- (readSudoku . getSudokuName) args
       let solver = getSolver args
       let s = solver sud
       printSudoku s
