module Public.P_updatePlayer (levelUp, incrementExp, newLayer, increaseMaxHp, updateHp, updateWeapon, updateArmour, updateHeal, updateMoney, updateLayer, updatePos, updateGoal, updatePrevdir) where

import Public.Consts.Structs

-- I Honeslty wish i found a better method for this, but such is life and Haskell :(
increaseMaxHp :: Player -> Int -> Player
increaseMaxHp inPlayer change = Player      (name inPlayer) change           (hp inPlayer) (weapon inPlayer) (armour inPlayer) (healpot inPlayer) (money inPlayer) (lowestLayer inPlayer) (playerPos inPlayer) (goal inPlayer) (start inPlayer) (prevDir inPlayer) (pExp inPlayer) (levelCap inPlayer)

updateHp :: Player -> Int -> Player
updateHp inPlayer change = Player           (name inPlayer) (maxHp inPlayer) (hp inPlayer+change) (weapon inPlayer) (armour inPlayer) (healpot inPlayer) (money inPlayer) (lowestLayer inPlayer) (playerPos inPlayer) (goal inPlayer) (start inPlayer) (prevDir inPlayer) (pExp inPlayer) (levelCap inPlayer)

updateWeapon :: Player -> Int -> Player
updateWeapon inPlayer change = Player       (name inPlayer) (maxHp inPlayer) (hp inPlayer) change (armour inPlayer) (healpot inPlayer) (money inPlayer) (lowestLayer inPlayer) (playerPos inPlayer) (goal inPlayer) (start inPlayer) (prevDir inPlayer) (pExp inPlayer) (levelCap inPlayer)

updateArmour :: Player -> Int -> Player
updateArmour inPlayer change = Player       (name inPlayer) (maxHp inPlayer) (hp inPlayer) (weapon inPlayer) change (healpot inPlayer) (money inPlayer) (lowestLayer inPlayer) (playerPos inPlayer) (goal inPlayer) (start inPlayer) (prevDir inPlayer) (pExp inPlayer) (levelCap inPlayer)

updateHeal :: Player -> Int -> Player
updateHeal inPlayer change = Player         (name inPlayer) (maxHp inPlayer) (hp inPlayer) (weapon inPlayer) (armour inPlayer) (healpot inPlayer+change) (money inPlayer) (lowestLayer inPlayer) (playerPos inPlayer) (goal inPlayer) (start inPlayer) (prevDir inPlayer) (pExp inPlayer) (levelCap inPlayer)

updateMoney :: Player -> Int -> Player
updateMoney inPlayer change = Player        (name inPlayer) (maxHp inPlayer) (hp inPlayer) (weapon inPlayer) (armour inPlayer) (healpot inPlayer) (money inPlayer+change) (lowestLayer inPlayer) (playerPos inPlayer) (goal inPlayer) (start inPlayer) (prevDir inPlayer) (pExp inPlayer) (levelCap inPlayer)

updateLayer :: Player -> Player
updateLayer inPlayer = Player               (name inPlayer) (maxHp inPlayer) (hp inPlayer) (weapon inPlayer) (armour inPlayer) (healpot inPlayer) (money inPlayer) (lowestLayer inPlayer+1) (playerPos inPlayer) (goal inPlayer) (start inPlayer) (prevDir inPlayer) (pExp inPlayer) (levelCap inPlayer)

updatePos :: Player -> Int -> Int -> Player
updatePos inPlayer newX newY = Player       (name inPlayer) (maxHp inPlayer) (hp inPlayer) (weapon inPlayer) (armour inPlayer) (healpot inPlayer) (money inPlayer) (lowestLayer inPlayer) (newX,newY) (goal inPlayer) (start inPlayer) (prevDir inPlayer) (pExp inPlayer) (levelCap inPlayer)

updateGoal :: Player -> Int -> Int -> Player
updateGoal inPlayer newX newY = Player      (name inPlayer) (maxHp inPlayer) (hp inPlayer) (weapon inPlayer) (armour inPlayer) (healpot inPlayer) (money inPlayer) (lowestLayer inPlayer) (playerPos inPlayer) (newX,newY) (start inPlayer) (prevDir inPlayer) (pExp inPlayer) (levelCap inPlayer)

updatePrevdir :: Player -> Int -> Player
updatePrevdir inPlayer newDir = Player      (name inPlayer) (maxHp inPlayer) (hp inPlayer) (weapon inPlayer) (armour inPlayer) (healpot inPlayer) (money inPlayer) (lowestLayer inPlayer) (playerPos inPlayer) (goal inPlayer) (start inPlayer) newDir (pExp inPlayer) (levelCap inPlayer)

newLayer :: Player -> (Int,Int) -> (Int,Int) -> Player
newLayer inPlayer (sX,sY) (gX,gY) = Player  (name inPlayer) (maxHp inPlayer+2) (hp inPlayer+2) (weapon inPlayer) (armour inPlayer) (healpot inPlayer) (money inPlayer) (lowestLayer inPlayer+1) (sX,sY) (gX,gY) (sX,sY) (prevDir inPlayer) (pExp inPlayer) (levelCap inPlayer)

incrementExp :: Player -> Int -> Player
incrementExp inPlayer inExp = Player        (name inPlayer) (maxHp inPlayer) (hp inPlayer) (weapon inPlayer) (armour inPlayer) (healpot inPlayer) (money inPlayer) (lowestLayer inPlayer) (playerPos inPlayer) (goal inPlayer) (start inPlayer) (prevDir inPlayer) (pExp inPlayer+inExp) (levelCap inPlayer)

levelUp :: Player -> (Int,Int) -> Player
levelUp inPlayer (sword, tank)  = Player    (name inPlayer) (maxHp inPlayer+2) (hp inPlayer+2) (weapon inPlayer+sword) (armour inPlayer+tank) (healpot inPlayer) (money inPlayer) (lowestLayer inPlayer) (playerPos inPlayer) (goal inPlayer) (start inPlayer) (prevDir inPlayer) (pExp inPlayer-levelCap inPlayer) (levelCap inPlayer*2)
