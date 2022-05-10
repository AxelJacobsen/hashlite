module MoveLoop.P_Move (checkForLegalMove) where

import System.Random ( Random(randomR), StdGen, mkStdGen, newStdGen )
import Structs(Player)
import Initialization.Pure.P_initChar(setMarkerEntry)

checkForLegalMove :: (Int,Int) -> [[Int]] -> StdGen -> ([[Int]], Bool)
checkForLegalMove (_,_) [[]] _ = ([[]],False)
checkForLegalMove (newX, newY) map seed
    | newX < 0 || newY < 0 || length map < newX || length map < newY = (map, False)
    | otherwise = do
        let newMap = setMarkerEntry (newX, newY) (length map, length map) 88 map [[]]
        (newMap, True)