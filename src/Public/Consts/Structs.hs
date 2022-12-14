module Public.Consts.Structs (Player(..),Enemy(..)) where         
data Player = Player { name::String, maxHp::Int, hp::Int, weapon::Int, armour::Int, healpot::Int, money::Int, lowestLayer::Int, playerPos::(Int,Int), goal::(Int,Int), start::(Int,Int), prevDir::Int, pExp::Int, levelCap::Int} deriving (Show, Eq)

data Enemy = Enemy {prefix::String, eName::String, eMaxHp::Int, eDamage::Int, eArmour::Int, eDrops::Int, expDrop::Int} deriving (Show, Eq)