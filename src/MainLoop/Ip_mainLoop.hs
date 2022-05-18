module MainLoop.Ip_mainLoop (gameLoop) where
import Structs (Player (..))
import System.Random ( Random(randomR), StdGen )
import System.IO ( hGetContents, openFile, IOMode(ReadMode) )
import System.Directory(doesFileExist)
import Control.Monad
import MainLoop.P_mainLoop(checkLegalIdleChoice)
import TextGeneral (levelUp1,levelUp2, continue,idleOptionsOne,idleOptionsTwo, exitGame, enterName,playerDeath1,playerDeath2,playerDeath3,playerDeath4)
import Consts ( idleOptionsList, playerNamePath)
import Data.Char
import Initialization.Impure.Ip_initChar (genNewFile)
import Initialization.Pure.P_initChar (generateCharacter, placeStartEnd)
import Initialization.Pure.MapGenerator (generateBoard, generateEmptyBoard)
import MoveLoop.Ip_Move(moveLoop)
import MoveLoop.Combat.P_combat(generateEnemy)
import MoveLoop.Combat.Ip_combat(combatLoop)
import Public.P_updatePlayer (incrementExp, updatePos, newLayer)
import Public.Ip_publicFuncs(checkLevelUp)

-- Main Loop
gameLoop :: Player -> Int -> [[Int]] -> ([[Int]], StdGen) -> IO ()
gameLoop player turnStep exploredMap (board,inSeed) --INITIALIZE CHARACTER
    | turnStep == -2 = do --New layer
        putStrLn ("You have found the entrance to the next level!\nYou gain "++show (lowestLayer player)++" EXP!")
        let newSize = 2+length exploredMap    --Increases Size of new map
        let (newExploredBoard, newSeed, startX, startY) = placeStartEnd newSize True (generateEmptyBoard newSize, inSeed) --Gets new start coords
        let (freshBoard, _) = generateBoard newSize 5 newSeed
        let (newFilledBoard, outSeed, goalX,goalY) = placeStartEnd newSize False (freshBoard, inSeed)                 -- Gets new goal coords
        newPlayer <- checkLevelUp (incrementExp (newLayer player (startX,startY) (goalX, goalY)) (lowestLayer player))
        gameLoop newPlayer 0 newExploredBoard (newFilledBoard, outSeed)
    | turnStep == -1 = do
        --Fills map with goal and 
        let (newExploredBoard, _, startX, startY) = placeStartEnd (length board) True (exploredMap, inSeed)
        let (newFilledBoard, outSeed, goalX,goalY) = placeStartEnd (length board) False (board, inSeed)
        fileExists <- doesFileExist playerNamePath
        if not fileExists then do
            putStrLn enterName
            newName <- getLine
            if null newName then gameLoop (generateCharacter "") turnStep newExploredBoard (newFilledBoard,outSeed)
            else do
                genNewFile playerNamePath newName --Stores new name into file
                let outPlayer = newLayer (generateCharacter newName) (startX, startY) (goalX, goalY)
                gameLoop outPlayer 0 newExploredBoard (newFilledBoard,outSeed) --Character name not null
        else do --File already exists
            handle <- openFile playerNamePath ReadMode
            contents <- hGetContents handle
            let charNameFile = head (lines contents)
            let outPlayer = newLayer (generateCharacter charNameFile) (startX, startY) (goalX, goalY)
            print (start outPlayer)
            print (playerPos outPlayer)
            print (goal outPlayer)
            gameLoop outPlayer 0 newExploredBoard (newFilledBoard,outSeed) --Player generated with file name
    --"MAIN MENU"
    | turnStep == 0 = do
        putStrLn (idleOptionsOne++name player++"?"++idleOptionsTwo)
        idleChoice <- getLine
        if null idleChoice then do
            gameLoop player turnStep exploredMap (board,inSeed)
        else gameLoop player (checkLegalIdleChoice (toLower (head idleChoice)) idleOptionsList 0) exploredMap (board,inSeed) --This should always be one of the leagl options,

    --Enters Move loops
    | turnStep == 1 = do
        (newPlayer, newlyExploredMap, dataMap, newSeed, boardTile) <- moveLoop player 0 (prevDir player) exploredMap (board,inSeed)
        case boardTile of
            -99 -> gameLoop newPlayer 0 newlyExploredMap (dataMap,newSeed)      --ERROR
            0 -> gameLoop newPlayer 0 newlyExploredMap (dataMap,newSeed)        --Quit FUNCTION
            3 -> do                                                             --COMBAT
                (loopPlayer, updatedDataMap, loopSeed, result) <- combatLoop newPlayer 0 (0,0) dataMap (generateEnemy inSeed (lowestLayer player))
                case result of
                    0 -> do
                        leveledPlayer <- checkLevelUp player
                        putStrLn "Press enter to continue..."
                        trash <- getLine
                        gameLoop leveledPlayer 0 newlyExploredMap (updatedDataMap,loopSeed)
                    1 -> gameLoop newPlayer 4 newlyExploredMap (updatedDataMap,loopSeed) 
                    _ -> gameLoop newPlayer 0 newlyExploredMap (updatedDataMap,loopSeed) 
            4 -> gameLoop newPlayer 0 newlyExploredMap (dataMap,newSeed)        --LOOT
            5 -> gameLoop newPlayer 0 newlyExploredMap (dataMap,newSeed)        --ENCOUNTER
            100 -> gameLoop newPlayer (-2) newlyExploredMap (dataMap,newSeed)   --NEXT LEVEL
            _ -> gameLoop newPlayer 0 newlyExploredMap (dataMap,newSeed)        --ALSO ERROR, SHOULDNT HAPPEN
    | turnStep == 2 = do
        print "REST LOOP"
        gameLoop player 0 exploredMap (board,inSeed)
    | turnStep == 3 = do
        putStrLn exitGame
        answer <- getLine
        if not (null answer) then do
            case toLower (head answer) of
                'y' -> putStrLn("Thanks for playing, see you later "++name player++"!")
                'n' -> gameLoop player 0 exploredMap (board,inSeed)
                _ -> gameLoop player 3 exploredMap (board,inSeed)
        else gameLoop player 3 exploredMap (board,inSeed)
    | turnStep == 4 = do    --You died
        putStrLn (name player++playerDeath1++name player++playerDeath2++show (money player)++playerDeath3++show (lowestLayer player)++playerDeath4)
    | otherwise = putStrLn "ERROR IN GAME LOOP"