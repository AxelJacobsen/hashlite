module Structs (Player(..)) where         
data Player = Player { name::String, maxHp::Int, hp::Int, weapon::Int, armour::Int, healpot::Int, money::Int, lowestLayer::Int, playerPos::(Int,Int), goal::(Int,Int), start::(Int,Int), prevDir::Int} deriving (Show)