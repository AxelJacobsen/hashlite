module MoveLoop.Ip_Move (moveLoop) where

import System.Random ( Random(randomR), StdGen)
import Structs(Player (..))
import TextGeneral(moveSymbols,moveOptions,moveIllegal)
import Data.Char (toLower)
import Public.P_updatePlayer (updatePos, updatePrevdir)
import MoveLoop.P_Move(checkForLegalMove,checkTileValue)

moveLoop :: Player -> Int -> Int -> [[Int]] -> ([[Int]], StdGen) -> IO (Player, [[Int]], [[Int]], StdGen, Int)
moveLoop player phase prevDir exploredMap (dataMap, inSeed)
    | phase == 0 = do-- Display Move Options and map
        putStrLn moveSymbols
        displayMap (length exploredMap) exploredMap
        moveLoop player 1 prevDir exploredMap (dataMap, inSeed)
    | phase == 1 = do
        putStrLn moveOptions
        moveDir <- getLine
        let (pposX,pposY) = playerPos player
        if not (null moveDir) then do
            case toLower (head moveDir) of
                --Move up
                'w' -> handleDirInput player 1 prevDir (pposX+1,pposY) exploredMap (dataMap, inSeed)
                --Move Left
                'a' -> handleDirInput player 2 prevDir (pposX,pposY-1) exploredMap (dataMap, inSeed)
                --Move down
                's' -> handleDirInput player 3 prevDir (pposX-1, pposY) exploredMap (dataMap, inSeed)
                --Move right
                'd' -> handleDirInput player 4 prevDir (pposX,pposY+1) exploredMap (dataMap, inSeed)
                'q' -> return (player, exploredMap, dataMap, inSeed, 0)
                _ -> do
                    putStrLn "Illegal input."
                    moveLoop player 1 prevDir exploredMap (dataMap, inSeed)
            else moveLoop player 1 prevDir exploredMap (dataMap, inSeed)
    | phase == 2 = do
        let boardTile = checkTileValue (playerPos player) dataMap
        case boardTile of
            -99 -> moveLoop player 1 prevDir exploredMap (dataMap, inSeed) --Error on tile, illegal pos
            3 -> do
                --ENTER COMBAT LOOP
                return (player, exploredMap, dataMap, inSeed, boardTile)
            4 -> do
                --ENTER LOOT LOOP
                moveLoop player 0 prevDir exploredMap (dataMap, inSeed) 
            5 -> do
                --ENTER ENCOUNTER LOOP
                moveLoop player 0 prevDir exploredMap (dataMap, inSeed) 
            100 -> do
                --EXIT LEVEL
                return (player, exploredMap, dataMap, inSeed, boardTile)
            _ -> moveLoop player 0 prevDir exploredMap (dataMap, inSeed)  -- Tile is empty
    | otherwise = return (player, exploredMap, dataMap, inSeed, -99)-- Exit due to error

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
            putStr " | "
            printMapRow row
        2 -> do
            putStr "---"
            printMapRow row
        3 -> do
            putStr " | "
            printMapRow row
        4 -> do
            putStr "---"
            printMapRow row
        11 -> do
            putStr " | "
            printMapRow row
        12 -> do
            putStr " +-"
            printMapRow row
        13 -> do
            putStr " | "
            printMapRow row
        14 -> do
            putStr "-+ "
            printMapRow row
        21 -> do
            putStr "-+ "
            printMapRow row
        22 -> do
            putStr "---"
            printMapRow row
        23 -> do
            putStr "-+ "
            printMapRow row
        24 -> do
            putStr "---"
            printMapRow row
        31 -> do
            putStr " | "
            printMapRow row
        32 -> do
            putStr " +-"
            printMapRow row
        33 -> do
            putStr " | "
            printMapRow row
        34 -> do
            putStr "-+ "
            printMapRow row
        41 -> do
            putStr " +-"
            printMapRow row
        42 -> do
            putStr "---"
            printMapRow row
        43 -> do
            putStr " +-"
            printMapRow row
        44 -> do
            putStr "---"
            printMapRow row
        88 -> do
            putStr " P "
            printMapRow row
        99 -> do
            putStr " S "
            printMapRow row
        100 -> do
            putStr " X "
            printMapRow row
        _ -> do
            putStr "   "
            printMapRow row

printLines :: Int -> IO()
printLines 0 = putStrLn "+"
printLines count = do putStr "---" ; printLines (count-1)


handleDirInput :: Player -> Int -> Int -> (Int,Int) -> [[Int]] -> ([[Int]], StdGen) -> IO (Player, [[Int]], [[Int]], StdGen, Int)
handleDirInput player piece prevDir (newX, newY) exploredMap (dataMap, inSeed) = do
    let (newMap,isLegal) = checkForLegalMove (newX,newY) 88 exploredMap inSeed
    if isLegal then do
        let newPlayer = updatePos player newX newY
        let outPlayer = updatePrevdir newPlayer piece
        if playerPos player /= start player then do--Sets old position to be directional piece, but only if not startpos
            let (outMap,isLegal) = checkForLegalMove (playerPos player) ((piece*10)+prevDir) newMap inSeed
            print ((piece*10)+prevDir)
            moveLoop outPlayer 2 piece outMap (dataMap, inSeed)
        else moveLoop outPlayer 2 piece newMap (dataMap, inSeed)
    else do
        putStrLn moveIllegal
        moveLoop player 1 prevDir exploredMap (dataMap, inSeed)