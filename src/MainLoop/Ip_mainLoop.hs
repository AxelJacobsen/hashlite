module MainLoop.Ip_mainLoop (gameLoop) where
import Structs (Player (..))
import System.Random ( Random(randomR), StdGen )
import System.IO ( hGetContents, openFile, IOMode(ReadMode) )
import System.Directory(doesFileExist)
import Control.Monad
import MainLoop.P_mainLoop(checkLegalIdleChoice,getListValue)
import TextGeneral ( idleOptionsOne,idleOptionsTwo, exitGame, enterName)
import Consts ( idleOptionsList, playerNamePath, mainPhases)
import Data.Char
import Initialization.Impure.Ip_initChar (genNewFile)
import Initialization.Pure.P_initChar (generateCharacter, placeStartEnd)
import Initialization.Pure.MapGenerator (generateBoard, generateEmptyBoard)
import MoveLoop.Ip_Move(moveLoop)
import Public.P_updatePlayer (updatePos, newLayer)

-- Main Loop
gameLoop :: Player -> Int -> [[Int]] -> ([[Int]], StdGen) -> IO ()
gameLoop player turnStep exploredMap (board,inSeed) --INITIALIZE CHARACTER
    | turnStep == getListValue mainPhases 5 = do --New layer
        let newSize = 2+length exploredMap    --Increases Size of new map
        let (newExploredBoard, newSeed, startX, startY) = placeStartEnd newSize True (generateEmptyBoard newSize, inSeed) --Gets new start coords
        let (freshBoard, _) = generateBoard newSize 5 newSeed
        let (newFilledBoard, outSeed, goalX,goalY) = placeStartEnd newSize False (freshBoard, inSeed)                 -- Gets new goal coords

        let newPlayer = newLayer player (startX,startY) (goalX, goalY)
        putStrLn ("You have leveld up, "++name player++"! MAXHP increased.")
        gameLoop newPlayer (getListValue mainPhases 1) newExploredBoard (newFilledBoard, outSeed)

    | turnStep == getListValue mainPhases 0 = do
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
                gameLoop outPlayer (getListValue mainPhases 1) newExploredBoard (newFilledBoard,outSeed) --Character name not null
        else do --File already exists
            handle <- openFile playerNamePath ReadMode
            contents <- hGetContents handle
            let charNameFile = head (lines contents)
            let outPlayer = newLayer (generateCharacter charNameFile) (startX, startY) (goalX, goalY)
            print (start outPlayer)
            print (playerPos outPlayer)
            print (goal outPlayer)
            gameLoop outPlayer (getListValue mainPhases 1) newExploredBoard (newFilledBoard,outSeed) --Player generated with file name

    --"MAIN MENU"
    | turnStep == getListValue mainPhases 1 = do
        putStrLn (idleOptionsOne++name player++"?"++idleOptionsTwo)
        idleChoice <- getLine
        if null idleChoice then do
            gameLoop player turnStep exploredMap (board,inSeed)

        else gameLoop player  (getListValue mainPhases (checkLegalIdleChoice (toLower (head idleChoice)) idleOptionsList 0)) exploredMap (board,inSeed) --This should always be one of the leagl options,

    --Enters Move loops
    | turnStep == getListValue mainPhases 2 = do
        (newPlayer, newlyExploredMap, fullMap, newSeed) <- moveLoop player 0 (prevDir player) exploredMap (board,inSeed)
        print newlyExploredMap
        gameLoop newPlayer (getListValue mainPhases 1) newlyExploredMap (fullMap,newSeed)
    | turnStep == getListValue mainPhases 3 = do
        print "REST LOOP"
        gameLoop player (getListValue mainPhases 1) exploredMap (board,inSeed)
    | turnStep == getListValue mainPhases 4 = do
        putStrLn exitGame
        answer <- getLine
        if not (null answer) then do
            case toLower (head answer) of
                'y' -> putStrLn("Thanks for playing, see you later "++name player++"!")
                'n' -> gameLoop player (getListValue mainPhases 1) exploredMap (board,inSeed)
                _ -> gameLoop player (getListValue mainPhases 4) exploredMap (board,inSeed)
        else gameLoop player (getListValue mainPhases 4) exploredMap (board,inSeed)
    | otherwise = putStrLn "ERROR IN GAME LOOP"