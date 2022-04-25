module Main where

import MapGenerator (generateBoard)
import System.Random (mkStdGen, newStdGen)

main :: IO ()
main = do
    originalSeed <- newStdGen
    print $ generateBoard 10 6 originalSeed
