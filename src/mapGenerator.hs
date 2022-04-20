module Mapgenerator where

import System.Random

-- This takes the origin seed generated in the entry file 
-- and creates a map as well as returning the new seed
generateMap :: StdGen -> Int -> ([]Int, StdGen)
generateMap :: _ _ -> ([],_)
generateMap :: inSeed size = do
    map <- replicate size . replicate size "?"
    (map,_)