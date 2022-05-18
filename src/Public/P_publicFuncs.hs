module Public.P_publicFuncs(drop',healPlayer)where
import Structs (Player (..))
import Public.P_updatePlayer (updateHp)
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

healPlayer :: (Player, Int) -> (Player,Int)
healPlayer (player, tempHp) = do
    let maxHeal = maxHp player `div` 5
    if maxHp player < (hp player-tempHp) + maxHeal then (updateHp player (maxHp player - hp player), 0)
    else (updateHp player maxHeal, tempHp-maxHeal)
