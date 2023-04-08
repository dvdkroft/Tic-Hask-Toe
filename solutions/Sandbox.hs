module Sandbox where
import A1
import A2
-- type Pokemon = (Name, Id, [Power])

-- type Company = (Name, Year, )

type Name       = String
type Id         = Int
type Power      = String

data Pokemon = MkPokemon Name Id [Power]
    deriving Show

pikachu = MkPokemon "Pikachu" 25 ["Electric"]
pikachu2 = MkPokemon "Pikachu" 25 ["Electric","Earth"]

getName :: Pokemon -> Name
getName (MkPokemon name _ _) = name

getPower :: Pokemon -> Power
getPower (MkPokemon _ _ (p : _)) = p

-- Key takeaway working with lists: there are only two components the 1. head 2. tail
fifth :: [a] -> a
fifth (_:(_:(_:(_:(x:_))))) = x

fst' :: (a,b,c) -> a
fst' (a,b,c) = a

-- Get the fifth element of a list
--fifth :: [a] -> a
--fifth (_:_:_:_:x:_) = x

-- Tuple

-- Replace a list
l1= [X,O,X]

s = splitAt 1 l1

e = head _EMPTY_BOARD_
t = last _TIED_BOARD_