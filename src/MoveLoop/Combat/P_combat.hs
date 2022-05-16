module MoveLoop.Combat.P_combat(generateEnemy) where

import Structs(Enemy(..))
import System.Random (Random(randomR), StdGen)


generateEnemy :: StdGen -> (Enemy, StdGen)
generateEnemy inSeed = do
        let (enemyType, strengthSeed) = randomR (0, (lowestLayer player)) inSeed :: (Int, StdGen) -- Generate enemy
        let (enemyStrength, outSeed) = randomR (0, 4) strengthSeed :: (Int, StdGen) -- Generate enemy strength
        ((generateEnemyInner enemyType enemyStrength),outSeed)

generateEnemyInner :: Int -> Int -> Enemy
generateEnemyInner enemyType statRng    --Enemy type will always be between 0 - and current player layer depth
    | enemyType == 0 = genSlime statRng
    | enemyType <= 3 = genGoblin statRng
    | enemyType <= 6 = genOrc statRng
    | enemyType <= 15 = genDrake statRng
    | enemyType <= 40 = genAbomination statRng

--Generates a slime type enemy
genSlime :: Int -> Enemy
genSlime 0 = genSlime 1
genSlime rng = Enemy{denotator = "a", eName = "Slime", eMaxHp = (3*rng), eHp = (3*rng), eDamage = (rng), eArmour = (rng-1), eDrops = (rng)}

--Generates a goblin type enemy
genGoblin :: Int -> Enemy
genGoblin 0 = genGoblin 1
genGoblin rng = Enemy{denotator = "a", eName = "Goblin", eMaxHp = (2*rng), eHp = (2*rng), eDamage = (2*rng), eArmour = (rng+1), eDrops = (rng+2)}

--Generates an Orc type enemy
genOrc :: Int -> Enemy
genOrc 0 = genOrc 1
genOrc rng = Enemy{denotator = "an", eName = "Orc", eMaxHp = (6*rng), eHp = (6*rng), eDamage = (3*rng), eArmour = (rng*2), eDrops = (rng*5)}

-- Generates a drake type enemy
genDrake :: Int -> Enemy
genDrake 0 = genDrake 1
genDrake rng = Enemy{denotator = "a", eName = "Drake", eMaxHp = (16*rng), eHp = (16*rng), eDamage = (6*rng), eArmour = (8*rng), eDrops = (rng*20)}

-- Generates an abomination
genAbomination :: Int -> Enemy
genAbomination 0 = genAbomination 1
genAbomination rng = Enemy{denotator = "an", eName = "Abomination", eMaxHp = (30*rng), eHp = (30*rng), eDamage = (12*rng), eArmour = (10*rng), eDrops = (rng*100)}
