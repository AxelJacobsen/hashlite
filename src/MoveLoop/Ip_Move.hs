module MoveLoop.Ip_Move (moveLoop) where

import System.Random ( Random(randomR), StdGen )
import Structs(Player)

moveLoop :: Player -> Int -> [[Int]] -> ([[Int]], StdGen) -> IO (Player, [[Int]], [[Int]], StdGen)
moveLoop player phase exploredMap (unexploredMap, inSeed)
    | phase == 0 = return (player, exploredMap, unexploredMap, inSeed)-- Display Move Options and map
    | phase == 1 = return (player, exploredMap, unexploredMap, inSeed)-- Handle actual movement
    | phase == 2 = return (player, exploredMap, unexploredMap, inSeed)-- What tile are you stainding on, call releveant function
    | otherwise = return (player, exploredMap, unexploredMap, inSeed)-- Exit due to error