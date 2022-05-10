module MoveLoop.P_Move (checkForLegalMove) where

import System.Random ( Random(randomR), StdGen, mkStdGen, newStdGen )
import Structs(Player)
import Initialization.Pure.P_initChar(setMarkerEntry)

--Checks if a given coordinate is on the map, and updates the map with the corresponding piece
checkForLegalMove :: (Int,Int) -> Int -> [[Int]] -> StdGen -> ([[Int]], Bool)
checkForLegalMove (_,_) _ [[]] _ = ([[]],False)
checkForLegalMove (newX, newY) piece map seed
    | newX < 0 || newY < 0 || length map <= newX || length map <= newY = (map, False)
    | otherwise = do
        let newMap = setMarkerEntry (newX, newY) (length map-1, length map) piece map [[]]
        (newMap, True)