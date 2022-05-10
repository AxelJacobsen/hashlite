module MoveLoop.Ip_Move (moveLoop) where

import System.Random ( Random(randomR), StdGen)
import Structs(Player (playerPos))
import TextGeneral(moveSymbols,moveOptions,moveIllegal)
import Data.Char (toLower)
import Public.P_updatePlayer (updatePos)
import MoveLoop.P_Move(checkForLegalMove)

moveLoop :: Player -> Int -> [[Int]] -> ([[Int]], StdGen) -> IO (Player, [[Int]], [[Int]], StdGen)
moveLoop player phase exploredMap (unexploredMap, inSeed)
    | phase == 0 = do-- Display Move Options and map
        putStrLn moveSymbols
        displayMap (length exploredMap) exploredMap
        print exploredMap
        moveLoop player 1 exploredMap (unexploredMap, inSeed)
    | phase == 1 = do
        putStrLn moveOptions
        moveDir <- getLine
        let (pposX,pposY) = playerPos player
        if not (null moveDir) then do
            case toLower (head moveDir) of
                --Move up
                'w' -> handleDirInput player (pposX,pposY+1) exploredMap (unexploredMap, inSeed)
                --Move Left
                'a' -> handleDirInput player (pposX+1,pposY) exploredMap (unexploredMap, inSeed)
                --Move down
                's' -> handleDirInput player (pposX, pposY-1) exploredMap (unexploredMap, inSeed)
                --Move right
                'd' -> handleDirInput player (pposX-1,pposY) exploredMap (unexploredMap, inSeed)
                _ -> do
                    putStrLn "Illegal input."
                    moveLoop player 1 exploredMap (unexploredMap, inSeed)
            else moveLoop player 1 exploredMap (unexploredMap, inSeed)
        return (player, exploredMap, unexploredMap, inSeed)-- Handle actual movement
    | phase == 2 = do
        putStrLn "Phase 3"
        return (player, exploredMap, unexploredMap, inSeed)-- What tile are you stainding on, call releveant function
    | otherwise = return (player, exploredMap, unexploredMap, inSeed)-- Exit due to error

-- SIZE OF MAP, MAP, DESIGNED TO PRINT MOVED PLAYER PATH
displayMap :: Int -> [[Int]] -> IO()
displayMap size [] = do putStr "+" ; printLines size ;
displayMap size (column:map)
    | size == (length map +1) = do
        putStr "+"
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
            putStr "-"
            printMapRow row
        2 -> do
            putStr "|"
            printMapRow row
        99 -> do
            putStr "S"
            printMapRow row
        88 -> do
            putStr "P"
            printMapRow row
        _ -> do
            putStr " "
            printMapRow row

printLines :: Int -> IO()
printLines 0 = putStrLn "+"
printLines count = do putStr "-" ; printLines (count-1)

handleDirInput :: Player -> (Int,Int) -> [[Int]] -> ([[Int]], StdGen) -> IO (Player, [[Int]], [[Int]], StdGen)
handleDirInput player (newX, newY) exploredMap (unexploredMap, inSeed) = do
    print newX
    print newY
    let (newMap,isLegal) = checkForLegalMove (newX,newY) exploredMap inSeed
    if isLegal then do
        let newPlayer = updatePos player newX newY
        moveLoop newPlayer 2 newMap (unexploredMap, inSeed)
    else do
        putStrLn moveIllegal
        moveLoop player 1 exploredMap (unexploredMap, inSeed)