module Public.P_publicFuncs(drop')where

-- | Returns list wiout the first x indexes
--
-- Examples:
--
-- >>> drop' 1 [1,1,1,-1,0,0,1,1,-1]
-- [1,1,-1,0,0,1,1,-1]
-- >>> drop' 4 [0,0,0,-1,-1,0,0,1,-1]
-- [-1,0,0,1,-1]
drop' :: Int -> [a] -> [a]
drop' _ [] = []
drop' 0 ys = ys
drop' x ys = drop' (x-1) (tail ys)