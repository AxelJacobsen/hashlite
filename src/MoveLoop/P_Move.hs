module MoveLoop.P_Move (checkForLegalMove,checkTileValue) where

import System.Random ( Random(randomR), StdGen, mkStdGen, newStdGen )
import Structs(Player)
import Initialization.Pure.P_initChar(setMarkerEntry)

--Checks if a given coordinate is on the map, and updates the map with the corresponding piece
checkForLegalMove :: (Int,Int) -> Int -> [[Int]] -> StdGen -> ([[Int]], Bool)
checkForLegalMove (_,_) _ [[]] _ = ([[]],False)
checkForLegalMove (newX, newY) piece map seed
    | newX < 0 || newY < 0 || length map <= newX || length map <= newY = (map, False)   --Trying to move out of bounds
    | checkTileValue (newX, newY) map == 99 = (map, True)                               --Trying to move onto start tile
    | otherwise = do
        let newMap = setMarkerEntry (newX, newY) (length map-1, length map) piece map [[]] --Gets the updated map
        (newMap, True)

--Takes current coords and fullMap, then returns value of current tile
checkTileValue :: (Int,Int) -> [[Int]] -> Int
checkTileValue (_, _) [[]] = -99
checkTileValue (_, _) [] = -99
checkTileValue (xpos, ypos) map
    | length map <= xpos || length map <= ypos || xpos < 0 || ypos < 0 = -99 --tile is within legal bounds
    | otherwise = map!!((length map-1)-xpos)!!ypos --