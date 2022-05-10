module MoveLoop.Ip_Move (moveLoop) where

import System.Random ( Random(randomR), StdGen )
import Structs(Player)
import TextGeneral(moveOptions)

moveLoop :: Player -> Int -> [[Int]] -> ([[Int]], StdGen) -> IO (Player, [[Int]], [[Int]], StdGen)
moveLoop player phase exploredMap (unexploredMap, inSeed)
    | phase == 0 = do
        putStrLn moveOptions
        displayMap (length exploredMap) exploredMap
        return (player, exploredMap, unexploredMap, inSeed)-- Display Move Options and map
    | phase == 1 = return (player, exploredMap, unexploredMap, inSeed)-- Handle actual movement
    | phase == 2 = return (player, exploredMap, unexploredMap, inSeed)-- What tile are you stainding on, call releveant function
    | otherwise = return (player, exploredMap, unexploredMap, inSeed)-- Exit due to error

-- SIZE OF MAP, MAP, DESIGNED TO PRINT MOVED PLAYER PATH
displayMap :: Int -> [[Int]] -> IO()
displayMap size [] = do putStr "|" ; printLines size
displayMap size (column:map)
    | size == (length map +1) = do
        putStr "|"
        printLines size
        putStr "|"
        printMapRow column
        putStrLn "|"
        displayMap size map
    | otherwise = do
        putStr "|"
        printMapRow column
        putStrLn "|"
        displayMap size map

printMapRow :: [Int] -> IO()
printMapRow [] = putStr ""
printMapRow (value:row) = do
    case value of
        1 -> do 
            putStr " - "
            printMapRow row
        2 -> do
            putStr " | "
            printMapRow row
        99 -> do
            putStr " S "
            printMapRow row
        88 -> do
            putStr " P "
            printMapRow row
        _ -> do
            putStr "   "
            printMapRow row

printLines :: Int -> IO()
printLines 0 = putStrLn "|"
printLines count = do putStr "---" ; printLines (count-1)