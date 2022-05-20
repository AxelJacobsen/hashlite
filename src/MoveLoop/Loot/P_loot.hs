module MoveLoop.Loot.P_loot(handleTrap, givePlayerLoot) where
import Public.Consts.Structs (Player (..))
import Public.P_updatePlayer (updateHp,updateArmour,updateWeapon,updateHeal)

handleTrap :: Player -> Player
handleTrap inPlayer = updateHp inPlayer (-maxHp inPlayer `div`5)

givePlayerLoot :: Player -> Int -> Player
givePlayerLoot player lootType
    | lootType == 0 = updateHeal player 3
    | lootType == 1 = updateArmour player 3
    | lootType == 2 = updateWeapon player 3
    | otherwise = updateHeal player 3