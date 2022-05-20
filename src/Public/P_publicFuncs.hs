module Public.P_publicFuncs(drop',healPlayer)where
import Public.Consts.Structs (Player (..))
import Public.P_updatePlayer (updateHp)

drop' :: Int -> [a] -> [a]
drop' _ [] = []
drop' 0 ys = ys
drop' x ys = drop' (x-1) (tail ys)

healPlayer :: (Player, Int) -> (Player,Int)
healPlayer (player, tempHp) = do
    let maxHeal = maxHp player `div` 5
    if maxHp player < (hp player-tempHp) + maxHeal then (updateHp player (maxHp player - hp player), 0)
    else (updateHp player maxHeal, tempHp-maxHeal)
