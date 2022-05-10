module Initialization.Pure.P_initChar (generateCharacter, placeStartEnd, setMarkerEntry) where
import Structs (Player (..))
import System.Random ( Random(randomR), StdGen )
import Public.P_publicFuncs(drop')
import Public.P_updatePlayer (updatePos)

generateCharacter :: String -> Player
generateCharacter name = Player {
    name = name,
    maxHp=5,    --Initialized with little HP because its immediatily increased
    hp=5,
    weapon  = 0,
    armour  = 0,
    healpot = 1,
    money   = 10,
    lowestLayer = 0,
    playerPos = (0,0),
    start = (0,0),
    goal = (0,0)}

--Handles getting limits for positioning
placeStartEnd :: Int -> Bool -> ([[Int]],StdGen) -> ([[Int]], StdGen, Int,Int)
placeStartEnd size isVisible (map, inseed) = do
    let randomLower = calcLower (size-1) 0.25
    let (startX, seedOne) = randomR (randomLower, size-1) inseed :: (Int, StdGen) --Generates starting position X
    let (startY, seedTwo) = randomR (randomLower, size-1) seedOne :: (Int, StdGen) --Generates starting position Y
    let (endX , seedThree) = randomR (1, size-1) seedTwo :: (Int, StdGen) --Generates exit position X
    --let mapWithStart = setStartRec (startX, startY) map ([[]], outSeed)
    let mapWithStart = setMarkerEntry (startX, startY) (size,size) 99 map [[]]

    if isVisible then (mapWithStart, inseed, startX, startY)
    else do
        if startY < size `div` 2 then do
            let (endY, outSeed) = randomR (size-1 `div` 2, size-1) seedThree :: (Int, StdGen)
            (setMarkerEntry (endX, endY) (size,size) 100 mapWithStart [[]], outSeed, endX, endY)
        else do
            let (endY, outSeed) = randomR (floor 1, (size-1)`div`2) seedThree :: (Int, StdGen)
            (setMarkerEntry (endX, endY) (size,size) 100 mapWithStart [[]] , outSeed, endX, endY)

--Inserts start and end marker into map 
setMarkerEntry :: (Int,Int) -> (Int,Int) -> Int -> [[Int]] -> [[Int]] -> [[Int]]
setMarkerEntry (_,_) (_,_) _ [[]]  map = map -- List has been iterated or was empty
setMarkerEntry (_,_) (_,_) _ []  map = map -- List has been iterated or was empty
setMarkerEntry (markX, markY) (sizeX, sizeY) marker (curMap:original) map
    | markX == sizeX && sizeX == 0 && map == [[]] = [setMarkerInner markY sizeY marker [] curMap]
    | markX == sizeX && sizeX == 0 = map++[setMarkerInner markY sizeY marker [] curMap]
    | markX == sizeX && map == [[]] = setMarkerEntry (markX, markY) (sizeX-1, sizeY) marker original [setMarkerInner markY sizeY marker [] curMap]--setStartRec (startX, startY) (endX, endY) (map, keepSeed)
    | markX == sizeX = setMarkerEntry (markX, markY) (sizeX-1, sizeY) marker original (map++[setMarkerInner markY sizeY marker [] curMap])--setStartRec (startX, startY) (endX, endY) (map, keepSeed)
    | map == [[]] = setMarkerEntry (markX, markY) (sizeX-1, sizeY) marker original [curMap]
    | otherwise = setMarkerEntry (markX, markY) (sizeX-1, sizeY) marker original (map++[curMap])

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