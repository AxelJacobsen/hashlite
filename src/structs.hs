module Structs (Player(..)) where         
data Player = Player { name::String, hp::Int, weapon::Int, armour::Int, healpot::Int, money::Int, lowestLayer::Int} deriving (Show)