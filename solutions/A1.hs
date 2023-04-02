{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
module A1 where

import Data.Char (toUpper)

-- *** Assignment 1-1 *** --

-- Q#01
_SIZE_ :: Int
_SIZE_ = 3

-- Q#02
_DISPLAY_LOGO_ :: Bool
_DISPLAY_LOGO_ = True

-- Q#03
convertRowIndex :: Char -> Int
convertRowIndex index = fromEnum(toUpper(index)) - 65

-- Q#04
_INVALID_MOVE_ :: (Int, Int)
_INVALID_MOVE_ = (-1,-1)

-- Q#05
_SEP_ :: [Char]
_SEP_ = ['_','|','_']

-- *** Assignment 1-2 *** --

-- Q#06

data Square = X | O | E
    deriving (Show, Eq)



-- Q#07
data GameState = Xwon | Owon | Tie | InProgres
    deriving (Show, Eq, Enum)


-- Q#08
type Player     =   Square
type Row        =   [Square]
type Line       =   [Square]
type Board      =   [Row]
type Move       =   (Int,Int)


-- Q#09
getFirstPlayer :: Bool -> Player
getFirstPlayer value =
    if value
        then X
        else O

getFirstPlayer_ :: Bool -> Player
getFirstPlayer_ value
    | value = X
    | not value = O


-- Q#10
showGameState :: GameState -> String
showGameState gs = case gs of
    Xwon        -> "X won"
    Owon        -> "O won"
    Tie         -> "Tie"
    InProgres   -> "In Progress"

-- Q#11
switchPlayer :: Player -> Player
switchPlayer player
    | player == X = O
    | player == O = X
    | player == E = E


-- Q#12
showSquare :: Square -> String
showSquare sq = case sq of
    X       -> "X"
    O       -> "O"
    E       -> "_"