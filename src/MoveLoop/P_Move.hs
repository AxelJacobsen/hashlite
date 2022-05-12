module MoveLoop.P_Move (checkForLegalMove,checkTileValue) where

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

--Takes current coords and fullMap, then returns value of current tile
checkTileValue :: (Int,Int) -> [[Int]] -> Int
checkTileValue (_, _) [[]] = -99
checkTileValue (_, _) [] = -99
checkTileValue (xpos, ypos) map
    | length map <= xpos || length map <= ypos || xpos < 0 || ypos < 0 = -99 --tile is within legal bounds
    | otherwise = map!!xpos!!ypos
