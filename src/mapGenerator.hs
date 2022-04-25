module MapGenerator (generateBoard) where 

import System.Random ( Random(randomR), StdGen )

generateBoard :: Int -> Int -> StdGen -> ([[Int]], StdGen)
generateBoard size maxVal = fillBoardWithValues' size size maxVal [[]]

fillBoardWithValues' :: Int -> Int -> Int -> [[Int]] -> StdGen -> ([[Int]], StdGen)
fillBoardWithValues' _ 0 _ map outSeed = (map, outSeed)
fillBoardWithValues' size count maxVal board inSeed = do
    let (mapCom, newSeed) = fillInnerBoard' size maxVal inSeed []
    if board == [[]] then fillBoardWithValues' size (count-1) maxVal [mapCom] newSeed 
    else fillBoardWithValues' size (count-1) maxVal (board++[mapCom]) newSeed

fillInnerBoard' :: Int -> Int -> StdGen -> [Int] -> ([Int], StdGen)
fillInnerBoard' 0 _ outSeed map = (map, outSeed)
fillInnerBoard' counter maxRngVal inSeed map = do
    let (randNum, outSeed) = randomR (0, maxRngVal) inSeed :: (Int, StdGen) -- x <= 3 == Empty, 4 == combat, 5 == chest 6 == encounter
    if randNum <= 3 then fillInnerBoard' (counter-1) maxRngVal outSeed (map++[0])
    else fillInnerBoard' (counter-1) maxRngVal outSeed (map++[randNum])