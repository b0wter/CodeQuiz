import Data.List (sortBy)
import Data.Char (digitToInt)

-- Column, row and box index: [1..9]
type Column = Int
type Row = Int
type Box = Int

allIndices :: [Int]
allIndices = [1 .. 9]

-- The value of a cell
type Digit = Char

-- Possible digits for each cell
allDigits :: [Digit]
allDigits = ['1' .. '9']

-- Possible input digits for each cell, including empty cells (= '0'|' ')
allInputDigits :: [Digit]
allInputDigits = ' ' : '0' : allDigits

-- A cell of the board with its Row, Column and Box index 
-- and either a list of candidates or the solved value:
data Cell = Cell Row Column Box (Either [Digit] Digit) deriving (Show)

-- The board consists of a list of cells
data Board = Board [Cell] deriving (Show)

-- Initial board: all digits are possible in every cell
initBoard :: Board
initBoard = Board [Cell r c (getBox r c) (Left allDigits)| r <- allIndices, c <- allIndices ]

-- Calculate the box index of a cell, given its row and column indices
getBox :: Row -> Column -> Box
getBox row col = (col - 1) `div` 3 + (row - 1) `div` 3 * 3 + 1

-- Wrapper to make a solved cell
mkCell :: Row -> Column -> Digit -> Cell
mkCell row col x 
    | col `elem` allIndices && 
      row `elem` allIndices && 
      x `elem` allDigits 
      = Cell row col (getBox row col) (Right x)
    | otherwise = error "Bad arguments to mkCell"

-- Set a cell in a board to a solved value and eliminate candidates in neighbouring cells
setCell :: Cell -> Board -> Board
setCell cell@(Cell r c b (Right x)) (Board cells) = Board (map set cells)
    where 
        set bcell@(Cell row col box xs) = 
            if col == c && row == r then cell                                            -- matches given cell
            else if col == c || row == r || box == b then (Cell row col box (sub xs))    -- cell on board is in same row, col or box as given cell: eliminate cell value from peers
            else bcell                                                                   -- cell on board is not influenced by given cell

        sub (Left xs) = Left ([y |y <- xs, y /= x])                                      -- eliminate candidates 
        sub (Right y) |  x == y = error (x : (" doesn't work (from " ++ (show r) ++ ", " ++ (show c)))                            -- peer cell has the same single value as given cell
        sub xx = xx                                                                      -- default
setCell _ _ = error "Bad arguments to setCell"

-- Get list of unsolved cells of a board
getUnsolved :: Board -> [Cell]
getUnsolved (Board cells) = [cell | cell@(Cell _ _ _ (Left _)) <- cells]

-- Compare two cells by the number of their candidates / options left
cmpLeft :: Cell -> Cell -> Ordering
cmpLeft (Cell _ _ _ (Left x)) (Cell _ _ _ (Left y)) = compare (length x) (length y)
cmpLeft _ _ = error "No candidates in cell"

-- Try to solve a board (lazy) and return all solutions, makes use of the list Monad
solveMany :: Board -> [Board]
solveMany board = 
    case getUnsolved board of 
    [] -> return board
    cells -> do
        let Cell r c b (Left xs) : _ = sortBy cmpLeft cells -- sort unsolved cells, get cell with least number of candidates
        cell <- [Cell r c b (Right x) | x <- xs]    -- go through all candidates for this cell
        solveMany (setCell cell board)              -- and try to solve with each

-- Get a string representation of a solved or unsolved board
showBoard :: Board -> String
showBoard board@(Board cells) = concat . map wrap $ cells
    where 
    wrap cell@(Cell r c b (Left xs))
        | c `mod` 9 == 0 = "0\n"
        | otherwise = "0"
    wrap cell@(Cell r c b (Right x))
        | c `mod` 9 == 0 = (++"\n") . show . digitToInt $ x 
        | otherwise = show . digitToInt $ x

-- From a solved board, return the sum of the first cells in the top 3 rows
getMagicNum :: Board -> Int
getMagicNum (Board cells) = sum [digitToInt d | (Cell r c _ (Right d)) <- cells, r `elem` [1..3], c == 1]

-- Get a string of statistics for solved boards
-- returns the full grid of the first solution, its magic number and the number of solutions
showStats :: [Board] -> String
showStats [] = ""
showStats boards@(first:_) = (showBoard first) ++ "M: "  ++ show (getMagicNum first) ++ " C: " ++ show (length boards)

-- Put a digit on a board, given the digit and its row and column coordinates
putDigit :: Board -> ((Int, Int), Digit) -> Board
putDigit brd ((row, col), digit) = setCell (mkCell row col digit) brd

-- Parse a starting grid (9 rows, separated by newlines, with at least 9 digits each)
parse :: String -> Board
parse = go . filter (`elem` allInputDigits)
    where 
    go input 
        | length input /= 81 = error "malformed input: need 9 lines with 9 digits on each line"
        | otherwise = foldl putDigit initBoard .                                -- put each digit on the initial board (eliminating peer candidates)
                      filter ((`elem` allDigits) . snd) .                       -- remove no-clue digits (0)
                      zip [ (r, c) | r <-allIndices, c<-allIndices ] $ input    -- zip digits with their coordinates
                      
-- Splits a list into chunks of n
chunksOf _ [] = []
chunksOf n list = first : (chunksOf n rest)
    where (first, rest) = splitAt n list

-- Read any input, break it into 10 lines (header + grid) 
-- and try to parse and solve, returning appropriate output
readAndSolve :: String -> String
readAndSolve = unlines . concat . map go . chunksOf 10 . lines
    where go (h:ls) = h : (lines . showStats . solveMany . parse . unlines $ ls)


-- Main reads and solves for all the input
main = interact $ readAndSolve

