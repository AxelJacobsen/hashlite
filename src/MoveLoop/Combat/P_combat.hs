module MoveLoop.Combat.P_combat(generateEnemy,enemyDamage,playerDamage) where

import Structs(Player(..), Enemy(..))
import System.Random (Random(randomR), StdGen)


generateEnemy :: StdGen -> Int -> (Enemy, StdGen)
generateEnemy inSeed typeCap = do
        let (enemyType, strengthSeed) = randomR (0, typeCap) inSeed :: (Int, StdGen) -- Generate enemy
        let (enemyStrength, outSeed) = randomR (1, 3) strengthSeed :: (Int, StdGen) -- Generate enemy strength
        (generateEnemyInner enemyType enemyStrength, outSeed)

generateEnemyInner :: Int -> Int -> Enemy
generateEnemyInner enemyType statRng    --Enemy type will always be between 0 - and current player layer depth
    | enemyType <= 0 = genSlime statRng
    | enemyType <= 3 = genGoblin statRng
    | enemyType <= 6 = genOrc statRng
    | enemyType <= 15 = genDrake statRng
    | enemyType <= 40 = genAbomination statRng
    | enemyType == 100 = genOddvarBraa statRng
    | otherwise = genAbomination statRng

--Generates a slime type enemy
genSlime :: Int -> Enemy
genSlime 0 = genSlime 1
genSlime rng = Enemy{prefix = "a ", eName = "Slime", eMaxHp = 3*rng, eDamage = 1, eArmour = rng, eDrops = rng+1, expDrop = 1}

--Generates a goblin type enemy
genGoblin :: Int -> Enemy
genGoblin 0 = genGoblin 1
genGoblin rng = Enemy{prefix = "a ", eName = "Goblin", eMaxHp = 2*rng, eDamage = 2+rng, eArmour = rng-1, eDrops = rng+2, expDrop = 2}

--Generates an Orc type enemy
genOrc :: Int -> Enemy
genOrc 0 = genOrc 1
genOrc rng = Enemy{prefix = "an ", eName = "Orc", eMaxHp = 10+(3*rng), eDamage = 6+(2*rng), eArmour = 2+rng*2, eDrops = (rng*5)+10, expDrop = 5}

-- Generates a drake type enemy
genDrake :: Int -> Enemy
genDrake 0 = genDrake 1
genDrake rng = Enemy{prefix = "a ", eName = "Drake", eMaxHp = 30+(16*rng), eDamage = 12+(6*rng), eArmour = 10+(8*rng), eDrops = (rng*20)+20, expDrop = 10}

-- Generates an abomination
genAbomination :: Int -> Enemy
genAbomination 0 = genAbomination 1
genAbomination rng = Enemy{prefix = "an ", eName = "Abomination", eMaxHp = 100+(30*rng), eDamage = 25+(12*rng), eArmour = 16+(10*rng), eDrops = (rng*100)+100, expDrop = 20}

-- Generates Oddvar Braa
genOddvarBraa :: Int -> Enemy
genOddvarBraa 0 = genOddvarBraa 1
genOddvarBraa rng = Enemy{prefix = "", eName = "Oddvar Braa", eMaxHp = 1000+(100*rng), eDamage = 100+(30*rng), eArmour = 10+(10*rng), eDrops = 2766342, expDrop = 1000}


enemyDamage :: Enemy -> Player -> StdGen -> (Int, StdGen, Bool)
enemyDamage enemy player inSeed = do
    let (hitReg, outSeed) = randomR (0, 10) inSeed :: (Int, StdGen) -- Check hit
    case hitReg of
        0 -> (0, outSeed, False)
        10 -> legalizeDamage ((eDamage enemy*2)-armour player, outSeed, True)
        _ -> legalizeDamage (eDamage enemy-armour player, outSeed, False)

playerDamage :: Enemy -> Player -> StdGen -> (Int, StdGen, Bool)
playerDamage enemy player inSeed = do
    let (hitReg, outSeed) = randomR (0, 10) inSeed :: (Int, StdGen) -- Check hit
    case hitReg of
        0 -> (0, outSeed, False)
        10 -> legalizeDamage ((weapon player*2)-eArmour enemy, outSeed, True)
        _ -> legalizeDamage (weapon player-eArmour enemy, outSeed, False)

legalizeDamage :: (Int, StdGen, Bool) -> (Int, StdGen, Bool)
legalizeDamage (damage,seed, crit)
    | damage <= 0 = (1,seed, crit) 
    | otherwise = (damage,seed, crit) 