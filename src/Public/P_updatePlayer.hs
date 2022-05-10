module Public.P_updatePlayer (increaseMaxHp, updateHp, updateWeapon, updateArmour, updateHeal, updateMoney, updateLayer, updatePos) where

import Structs

increaseMaxHp :: Player -> Int -> Player
increaseMaxHp inPlayer change = Player (name inPlayer) change (hp inPlayer) (weapon inPlayer) (armour inPlayer) (healpot inPlayer) (money inPlayer) (lowestLayer inPlayer) (playerPos inPlayer)

updateHp :: Player -> Int -> Player
updateHp inPlayer change = Player (name inPlayer) (maxHp inPlayer) (hp inPlayer+change) (weapon inPlayer) (armour inPlayer) (healpot inPlayer) (money inPlayer) (lowestLayer inPlayer) (playerPos inPlayer)

updateWeapon :: Player -> Int -> Player
updateWeapon inPlayer change = Player (name inPlayer) (maxHp inPlayer) (hp inPlayer) change (armour inPlayer) (healpot inPlayer) (money inPlayer) (lowestLayer inPlayer) (playerPos inPlayer)

updateArmour :: Player -> Int -> Player
updateArmour inPlayer change = Player (name inPlayer) (maxHp inPlayer) (hp inPlayer) (weapon inPlayer) change (healpot inPlayer) (money inPlayer) (lowestLayer inPlayer) (playerPos inPlayer)

updateHeal :: Player -> Int -> Player
updateHeal inPlayer change = Player (name inPlayer) (maxHp inPlayer) (hp inPlayer) (weapon inPlayer) (armour inPlayer) change (money inPlayer) (lowestLayer inPlayer) (playerPos inPlayer)

updateMoney :: Player -> Int -> Player
updateMoney inPlayer change = Player (name inPlayer) (maxHp inPlayer) (hp inPlayer) (weapon inPlayer) (armour inPlayer) (healpot inPlayer) change (lowestLayer inPlayer) (playerPos inPlayer)

updateLayer :: Player -> Int -> Player
updateLayer inPlayer change = Player (name inPlayer) (maxHp inPlayer) (hp inPlayer) (weapon inPlayer) (armour inPlayer) (healpot inPlayer) (money inPlayer) (lowestLayer inPlayer+1) (playerPos inPlayer)

updatePos :: Player -> Int -> Int -> Player
updatePos inPlayer newX newY = Player (name inPlayer) (maxHp inPlayer) (hp inPlayer) (weapon inPlayer) (armour inPlayer) (healpot inPlayer) (money inPlayer) (lowestLayer inPlayer) (newX,newY)