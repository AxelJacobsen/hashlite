module Main where

import Initialization.Pure.P_MapGenerator (generateBoard, generateEmptyBoard)
import Initialization.Pure.P_initChar (generateCharacter)
import System.Random (mkStdGen, newStdGen)
import Public.Consts.Structs (Player)
import MainLoop.Ip_mainLoop (gameLoop)
import System.IO
import Public.Consts.TextGeneral(pressEnter, helpFlag1, helpFlag2, helpFlag3, continue)
import System.Directory.Internal.Prelude (getArgs)
import Data.Char(toLower)
import Public.Consts.Logo(logoText)

main :: IO ()
main = do
    needHelp <- getArgs 
    let acceptAbleOptions = ["--help","-help","help","--h","-h","h"]
    let getHelp = if not (null needHelp) && (map toLower (head needHelp) `elem` acceptAbleOptions) then do putStrLn (helpFlag1++helpFlag2++helpFlag3) ; putStrLn continue ; _ <- getLine; putStr "" else putStr ""
    getHelp
    originalSeed <- newStdGen
    printLogo logoText
    putStrLn pressEnter
    _ <- getLine
    gameLoop (generateCharacter "") (-1) (generateEmptyBoard 5) (generateBoard 5 5 originalSeed)

printLogo :: []String -> IO ()
printLogo [] = putStrLn ""
printLogo (line:rest) = do putStrLn line ; printLogo rest