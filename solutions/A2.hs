{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

module A2 where

import A1
import Data.List (intercalate)
import Data.IORef (newIORef)

-- *** Assignment 2-1 *** --

-- Q#01
promptPlayer :: Player -> String
promptPlayer p = concat ["Player ",showSquare p, "'s turn: enter a row and column position (ex. A1)"]

-- Q#02
_RANGE_ :: [Int]
_RANGE_ = [0 .. _SIZE_-1]

-- Q#03
isDigit :: Char -> Bool
isDigit c = c `elem` ['0'..'9']

readDigit :: Char -> Int
readDigit c
    | isDigit c = read [c]
    | otherwise = -1

-- Q#04

_EMPTY_ROW_ = replicate _SIZE_ E


_EMPTY_BOARD_ = replicate _SIZE_ _EMPTY_ROW_

-- Q#05
isTied :: Board -> Bool
isTied b = notElem E (concat b)


_TIED_BOARD_ :: Board
_TIED_BOARD_ = [[X, O, O],[O, X, X],[O, X, O]]


-- Q#06
indexRowStrings :: [String] -> [(Char,String)]
indexRowStrings input = zip ['A' ..] input

-- Q#07
formatLine :: [String] -> String
formatLine input = concat [_SEP_,intercalate _SEP_ input,_SEP_]

-- *** Assignment 2-2 *** --

-- Q#08
isMoveInBounds :: Move -> Bool
--isMoveInBounds (x,y) = x>=0 && x<=_SIZE_ && y >= 0 && y <= _SIZE_
isMoveInBounds (x,y) = and [x>=0, x<=_SIZE_, y >= 0, y <= _SIZE_]

-- Q#09
stringToMove :: String -> Move
stringToMove [] = _INVALID_MOVE_
stringToMove [x,y] = (convertRowIndex x, readDigit y)
stringToMove _ = _INVALID_MOVE_


-- Q#10
replaceSquareInRow :: Player -> Int -> Row -> Row
replaceSquareInRow p c r 
    | c < 0 || c >= _SIZE_  = r
    | null r                = []
    | otherwise             = x ++ p : xs
        where (x,_:xs) = splitAt c r
rsX = replaceSquareInRow X
rsO = replaceSquareInRow O