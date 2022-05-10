module Public.P_updatePlayer (newLayer, increaseMaxHp, updateHp, updateWeapon, updateArmour, updateHeal, updateMoney, updateLayer, updatePos, updateGoal) where

import Structs

increaseMaxHp :: Player -> Int -> Player
increaseMaxHp inPlayer change = Player (name inPlayer) change (hp inPlayer) (weapon inPlayer) (armour inPlayer) (healpot inPlayer) (money inPlayer) (lowestLayer inPlayer) (playerPos inPlayer) (goal inPlayer)

updateHp :: Player -> Int -> Player
updateHp inPlayer change = Player (name inPlayer) (maxHp inPlayer) (hp inPlayer+change) (weapon inPlayer) (armour inPlayer) (healpot inPlayer) (money inPlayer) (lowestLayer inPlayer) (playerPos inPlayer) (goal inPlayer)

updateWeapon :: Player -> Int -> Player
updateWeapon inPlayer change = Player (name inPlayer) (maxHp inPlayer) (hp inPlayer) change (armour inPlayer) (healpot inPlayer) (money inPlayer) (lowestLayer inPlayer) (playerPos inPlayer) (goal inPlayer)

updateArmour :: Player -> Int -> Player
updateArmour inPlayer change = Player (name inPlayer) (maxHp inPlayer) (hp inPlayer) (weapon inPlayer) change (healpot inPlayer) (money inPlayer) (lowestLayer inPlayer) (playerPos inPlayer) (goal inPlayer)

updateHeal :: Player -> Int -> Player
updateHeal inPlayer change = Player (name inPlayer) (maxHp inPlayer) (hp inPlayer) (weapon inPlayer) (armour inPlayer) change (money inPlayer) (lowestLayer inPlayer) (playerPos inPlayer) (goal inPlayer)

updateMoney :: Player -> Int -> Player
updateMoney inPlayer change = Player (name inPlayer) (maxHp inPlayer) (hp inPlayer) (weapon inPlayer) (armour inPlayer) (healpot inPlayer) change (lowestLayer inPlayer) (playerPos inPlayer) (goal inPlayer)

updateLayer :: Player -> Player
updateLayer inPlayer = Player (name inPlayer) (maxHp inPlayer) (hp inPlayer) (weapon inPlayer) (armour inPlayer) (healpot inPlayer) (money inPlayer) (lowestLayer inPlayer+1) (playerPos inPlayer) (goal inPlayer)

updatePos :: Player -> Int -> Int -> Player
updatePos inPlayer newX newY = Player (name inPlayer) (maxHp inPlayer) (hp inPlayer) (weapon inPlayer) (armour inPlayer) (healpot inPlayer) (money inPlayer) (lowestLayer inPlayer) (newX,newY) (goal inPlayer)

updateGoal :: Player -> Int -> Int -> Player
updateGoal inPlayer newX newY = Player (name inPlayer) (maxHp inPlayer) (hp inPlayer) (weapon inPlayer) (armour inPlayer) (healpot inPlayer) (money inPlayer) (lowestLayer inPlayer) (playerPos inPlayer) (newX,newY)

newLayer :: Player -> (Int,Int) -> (Int,Int) -> Player
newLayer inPlayer (startX,startY) (goalX,goalY) = Player (name inPlayer) (maxHp inPlayer+5) (hp inPlayer+5) (weapon inPlayer) (armour inPlayer) (healpot inPlayer) (money inPlayer) (lowestLayer inPlayer+1) (startX,startY) (goalX,goalY)
