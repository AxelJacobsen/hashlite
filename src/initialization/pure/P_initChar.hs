module Initialization.Pure.P_initChar (generateCharacter, placeStartEnd) where
import Structs (Player (..))
import System.Random ( Random(randomR), StdGen )
import Public.P_publicFuncs(drop')

generateCharacter :: String -> Player
generateCharacter name = Player {
    name = name,
    hp=10,
    weapon  = 0,
    armour  = 0,
    healpot = 1,
    money   = 10,
    lowestLayer = 0}

--Handles getting limits for positioning
placeStartEnd :: Int -> Bool -> ([[Int]],StdGen) -> ([[Int]],StdGen)
placeStartEnd size isVisible (map, inseed) = do
    let randomLower = calcLower size 0.25
    let (startX, seedOne) = randomR (randomLower, size) inseed :: (Int, StdGen) --Generates a starting position
    let (startY, seedTwo) = randomR (randomLower, size) seedOne :: (Int, StdGen) --Generates a starting position
    let (endX , seedThree) = randomR (0, size) seedTwo :: (Int, StdGen) --Generates a starting position
    --let mapWithStart = setStartRec (startX, startY) map ([[]], outSeed)
    let (mapWithStart, _) = setMarkerEntry (startX, startY) (size,size) 99 map ([[]], seedThree)

    if isVisible then (mapWithStart, inseed) 
    else do
        if startY < size `div` 2 then do
            let (endY , outSeed) = randomR (size `div` 2, size) seedThree :: (Int, StdGen)
            setMarkerEntry (endX, endY) (size,size) 100 mapWithStart ([[]], outSeed)
        else do
            let (endY , outSeed) = randomR (floor 0, size`div`2) seedThree :: (Int, StdGen)
            setMarkerEntry (endX, endY) (size,size) 100 mapWithStart ([[]], outSeed)

--Inserts start and end marker into map 
setMarkerEntry :: (Int,Int) -> (Int,Int) -> Int -> [[Int]] -> ([[Int]],StdGen) -> ([[Int]],StdGen)
setMarkerEntry (_,_) (_,_) _ [[]]  (map,seed) = (map,seed) -- List has been iterated or was empty
setMarkerEntry (_,_) (_,_) _ []  (map,seed) = (map,seed) -- List has been iterated or was empty
setMarkerEntry (markX, markY) (sizeX, sizeY) marker (curMap:original) (map, keepSeed)
    | markX == sizeX && sizeX == 0 && map == [[]] = ([setMarkerInner markY sizeY marker [] curMap], keepSeed)
    | markX == sizeX && sizeX == 0 = (map++[setMarkerInner markY sizeY marker [] curMap], keepSeed)
    | markX == sizeX = setMarkerEntry (markX, markY) (sizeX-1, sizeY) marker original (map++[setMarkerInner markY sizeY marker [] curMap], keepSeed)--setStartRec (startX, startY) (endX, endY) (map, keepSeed)
    | map == [[]] = setMarkerEntry (markX, markY) (sizeX-1, sizeY) marker original ([curMap], keepSeed)
    | otherwise = setMarkerEntry (markX, markY) (sizeX-1, sizeY) marker original (map++[curMap], keepSeed)


--Replaces nth value safely
setMarkerInner :: Int -> Int -> Int -> [Int] -> [Int] -> [Int]
setMarkerInner _ _ _ outMap [] = outMap
setMarkerInner markPos fullLength marker curMap (mapIt:inMap) 
    | markPos == 0 && fullLength == 0 && curMap /= [] = curMap++[marker]
    | markPos == 0 && fullLength == 0 = [marker]
    | fullLength == 0 = curMap
    | markPos == 0 = setMarkerInner (markPos-1) (fullLength-1) marker (curMap++[marker]) inMap
    | otherwise = setMarkerInner (markPos-1) (fullLength-1) marker (curMap++[mapIt]) inMap

--Gets lower rng limit for startPos
calcLower :: Int -> Float -> Int
calcLower size fract =  size - floor (fract * fromIntegral size)