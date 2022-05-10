module MainLoop.Ip_mainLoop (gameLoop) where
import Structs (Player (Player, name))
import System.Random ( Random(randomR), StdGen )
import System.IO ( hGetContents, openFile, IOMode(ReadMode) )
import System.Directory(doesFileExist)
import Control.Monad
import MainLoop.P_mainLoop(checkLegalIdleChoice,getListValue)
import TextGeneral ( idleOptions, exitGame)
import Consts ( idleOptionsList, playerNamePath, mainPhases)
import Data.Char
import Initialization.Impure.Ip_initChar (genNewFile)
import Initialization.Pure.P_initChar (generateCharacter, placeStartEnd)
import MoveLoop.Ip_Move(moveLoop)

-- Main Loop
gameLoop :: Player -> Int -> [[Int]] -> ([[Int]], StdGen) -> IO ()
gameLoop player turnStep exploredMap (board,inSeed) --INITIALIZE CHARACTER
    | turnStep == getListValue mainPhases 0 = do
        --Fills map with goal and 
        let (newExploredBoard, newSeed) = placeStartEnd (length board) True (exploredMap, inSeed)
        let (newFilledBoard, outSeed) = placeStartEnd (length board) False (board, newSeed)
        fileExists <- doesFileExist playerNamePath
        if not fileExists then do
            newName <- getLine
            if null newName then gameLoop (generateCharacter "") turnStep newExploredBoard (newFilledBoard,outSeed)
            else do
                genNewFile playerNamePath newName --Stores new name into file
                gameLoop (generateCharacter newName) (getListValue mainPhases 1) newExploredBoard (newFilledBoard,outSeed) --Character name not null
        else do --File already exists
            handle <- openFile playerNamePath ReadMode
            contents <- hGetContents handle
            let charNameFile = head (lines contents)
            gameLoop (generateCharacter charNameFile) (getListValue mainPhases 1) newExploredBoard (newFilledBoard,outSeed) --Player generated with file name
    
    --"MAIN MENU"
    | turnStep == getListValue mainPhases 1 = do
        putStrLn idleOptions
        idleChoice <- getLine
        if null idleChoice then do
            gameLoop player turnStep exploredMap (board,inSeed)
            --checkLegal bellow will always return one of the "legal" options
        else gameLoop player (getListValue mainPhases (checkLegalIdleChoice (toLower (head idleChoice)) idleOptionsList 0)) exploredMap (board,inSeed) --This should always be one of the leagl options,

    --Enters Move loops
    | turnStep == getListValue mainPhases 2 = do
        print "MOVE LOOP"
        (newPlayer, newlyExploredMap, fullMap, newSeed) <- moveLoop player 0 exploredMap (board,inSeed)
        gameLoop newPlayer (getListValue mainPhases 1) newlyExploredMap (fullMap,newSeed)
    | turnStep == getListValue mainPhases 3 = do
        print "REST LOOP"
        gameLoop player (getListValue mainPhases 1) exploredMap (board,inSeed)
    | turnStep == getListValue mainPhases 4 = do
        print exitGame
        answer <- getLine
        case answer of
            "y" -> print "Thanks for playing, see you later!"
            "n" -> gameLoop player (getListValue mainPhases 1) exploredMap (board,inSeed)
            _ -> gameLoop player (getListValue mainPhases 4) exploredMap (board,inSeed)
    | otherwise = putStrLn "ERROR IN GAME LOOP"