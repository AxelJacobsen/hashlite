module Main where

import Initialization.Pure.MapGenerator (generateBoard)
import Initialization.Pure.P_initChar (generateCharacter)
import System.Random (mkStdGen, newStdGen)
import Structs (Player)
import MainLoop.Ip_mainLoop (gameLoop)
import System.IO
import TextGeneral(pressEnter)

main :: IO ()
main = do
    originalSeed <- newStdGen
    --CHECK FILE FOR EXISTING PLAYER FIRST
    contents <- readFile "logo.txt"
    print contents
    print pressEnter
    _ <- getLine
    --IF NO PLAYER EXISTS ASK FOR NAME AND START
    gameLoop (generateCharacter "") (-1) (generateBoard 10 5 originalSeed)
    --