module A3 where

import A1
import A2

import Data.List (transpose)

-- *** Assignment 3-1 ***

-- Q#01
showInts :: [Int] -> [String]
showInts [] = []
showInts (x:xs) = show x : showInts xs


_HEADER_ = " " ++ formatLine (showInts _RANGE_)

-- Q#02
showSquares :: [Square] -> [String]
showSquares [] = []
showSquares (x:xs) = showSquare x : showSquares xs


-- Q#03
formatRows :: [Row] -> [String]
formatRows [] = []
formatRows (x:xs) = formatLine (showSquares x) : formatRows xs


-- Q#04
isColEmpty :: Row -> Int -> Bool
isColEmpty [] _             = False
isColEmpty (x:xs) 0         = x == E
isColEmpty (_:xs) colIndex  = isColEmpty xs (colIndex-1)


-- Q#05
dropFirstCol :: Board -> Board
dropFirstCol = map tail

dropLastCol :: Board -> Board
dropLastCol = map init

-- Q#06
getDiag1 :: Board -> Line
getDiag1 [] = []
getDiag1 ([]:_) = []
getDiag1 ((x:_):xs) = x : getDiag1 (map tail xs)

getDiag2 :: Board -> Line
getDiag2 [] = []
getDiag2 ([]:_) = []
getDiag2 rows = last (map init rows)

getAllLines :: Board -> [Line]
getAllLines board = horizontalLines ++ verticalLines ++ diagonalLines
  where
    horizontalLines = board
    verticalLines = transpose board
    diagonalLines = [getDiag1 board, getDiag2 board]
-- *** Assignment 3-2 ***

-- Q#07

putSquare = undefined

-- Q#08

prependRowIndices = undefined

-- Q#09

isWinningLine = undefined

-- Q#10

isValidMove = undefined