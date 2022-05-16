module MoveLoop.Combat.Ip_combat where
import Structs (Player (..))
import System.Random ( Random(randomR), StdGen )
import System.IO ( hGetContents, openFile, IOMode(ReadMode) )
import System.Directory(doesFileExist)
import Control.Monad
import MainLoop.P_mainLoop(checkLegalIdleChoice)
import TextGeneral ( idleOptionsOne,idleOptionsTwo, exitGame, enterName)
import Consts ( idleOptionsList, playerNamePath)
import Data.Char
import Initialization.Impure.Ip_initChar (genNewFile)
import Initialization.Pure.P_initChar (generateCharacter, placeStartEnd)
import Initialization.Pure.MapGenerator (generateBoard, generateEmptyBoard)
import MoveLoop.Ip_Move(moveLoop)
import Public.P_updatePlayer (updatePos, newLayer)

-- I want this to go right into the loop to skip unecessary nests
-- therefore creature type has to be predecided
combatLoop :: Player -> Int -> [[Int]] -> (Enemy, StdGen) -> IO (Player, [[Int]], StdGen)
combatLoop player phase unexploredMap (enemy, inSeed)
    | phase == 0 = do-- Display Move Options and map

        putStrLn moveSymbols
        displayMap (length exploredMap) exploredMap
        moveLoop player 1 prevDir exploredMap (unexploredMap, inSeed)
    | phase == 1 = do
        putStrLn moveOptions
        moveDir <- getLine
        let (pposX,pposY) = playerPos player
        if not (null moveDir) then do
            case toLower (head moveDir) of
                --Move up
                'w' -> handleDirInput player 1 prevDir (pposX+1,pposY) exploredMap (unexploredMap, inSeed)
                --Move Left
                'a' -> handleDirInput player 2 prevDir (pposX,pposY-1) exploredMap (unexploredMap, inSeed)
                --Move down
                's' -> handleDirInput player 3 prevDir (pposX-1, pposY) exploredMap (unexploredMap, inSeed)
                --Move right
                'd' -> handleDirInput player 4 prevDir (pposX,pposY+1) exploredMap (unexploredMap, inSeed)
                _ -> do
                    putStrLn "Illegal input."
                    moveLoop player 1 prevDir exploredMap (unexploredMap, inSeed)
            else moveLoop player 1 prevDir exploredMap (unexploredMap, inSeed)
    | phase == 2 = do
        let boardTile = checkTileValue (playerPos player) unexploredMap
        case boardTile of
            -99 -> moveLoop player 1 prevDir exploredMap (unexploredMap, inSeed) --Error on tile, illegal pos
            3 -> do
                --ENTER COMBAT LOOP
                return (player, exploredMap, unexploredMap, inSeed, boardTile)
            4 -> do
                --ENTER LOOT LOOP
                return (player, exploredMap, unexploredMap, inSeed, boardTile)
            5 -> do
                --ENTER ENCOUNTER LOOP
                return (player, exploredMap, unexploredMap, inSeed, boardTile)
            100 -> do
                --EXIT LEVEL
                return (player, exploredMap, unexploredMap, inSeed, boardTile)
            _ -> return (player, exploredMap, unexploredMap, inSeed, boardTile) -- Tile is empty
    | otherwise = return (player, exploredMap, unexploredMap, inSeed, -99)-- Exit due to error