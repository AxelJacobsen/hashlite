module Main where

import MapGenerator (generateBoard)
import System.Random (mkStdGen, newStdGen)
import Structs (Player)
import InitializeCharacter (generateCharacter)
import Ip_mainLoop (gameLoop)
import System.IO

main :: IO ()
main = do
    originalSeed <- newStdGen
    --CHECK FILE FOR EXISTING PLAYER FIRST
    --IF NO PLAYER EXISTS ASK FOR NAME AND START
    gameLoop (generateCharacter "" "") (generateBoard 10 5 originalSeed)
