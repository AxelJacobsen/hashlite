module MoveLoop.Combat.Ip_combat where
import Structs (Player (..), Enemy(..))
import System.Random ( Random(randomR), StdGen )
import System.IO ( hGetContents, openFile, IOMode(ReadMode) )
import MoveLoop.Combat.CombatText (killedEnemy1,killedEnemy2,killedEnemy3,killedEnemy4, encounterEnemy, coinSuccess, coinNeutral, coinCritical, escape, pAttack1, pAttack2, eAttack, damage, escapeFail, escapeSuccess, combatOptions1, combatOptions2)
import TextGeneral(healMessage,outOfHeal)
import Public.P_updatePlayer (updatePos, newLayer, updateHp, updateHeal,updateMoney, incrementExp)
import Data.Char (toLower)
import Public.P_publicFuncs(healPlayer)
import Public.Ip_publicFuncs(clearConsole)
import MoveLoop.Combat.P_combat(enemyDamage,playerDamage)

-- I want this to go right into the loop to skip unecessary nests
-- therefore creature type has to be predecided
combatLoop :: Player -> Int -> (Int,Int) -> [[Int]] -> (Enemy, StdGen) -> IO (Player, [[Int]], StdGen, Int)
combatLoop player phase (lEhp, lPhp) dataMap (enemy, inSeed) -- lehp and phph are temporary damage counters to reduce function calls
    | phase == 0 = do-- Enter combat encounter
        let (coinFlip, outSeed) = randomR (0, 6) inSeed :: (Int, StdGen)
        case coinFlip of
            0 -> do
                putStrLn (coinCritical++prefix enemy++eName enemy++"!")
                combatLoop player 2 (lEhp, lPhp) dataMap (enemy, inSeed)
            6 -> do
                putStrLn (encounterEnemy++prefix enemy++eName enemy)
                putStrLn (coinSuccess++escape)
                combatLoop player 8 (lEhp, lPhp) dataMap (enemy, inSeed)
            _ -> do
                putStrLn (encounterEnemy++prefix enemy++eName enemy)
                putStrLn coinNeutral
                combatLoop player 1 (lEhp, lPhp) dataMap (enemy, inSeed)
    | phase == 1 = do   --Player Turn
        printHp (name player) (eName enemy) (maxHp player-lPhp) (eMaxHp enemy-lEhp)
        putStr combatOptions1
        putStr (show (healpot player))
        putStrLn combatOptions2
        pInput <- getLine
        if null pInput then combatLoop player 1 (lEhp, lPhp) dataMap (enemy, inSeed)
        else case toLower (head pInput) of
            'a' -> do 
                let (dealtDamage, outSeed) = playerDamage enemy player inSeed
                clearConsole
                pDamagePrint player dealtDamage
                checkDeath player enemy 2 (lEhp+dealtDamage, lPhp) dataMap outSeed
            'h' -> do
                if 0 < healpot player then do
                    let healedPlayer = healPlayer (updateHeal player (-1))
                    putStrLn (healMessage++ show (hp healedPlayer - hp player) ++"HP.")
                    combatLoop player 2 (lEhp, lPhp) dataMap (enemy, inSeed)
                else do 
                    putStrLn outOfHeal
                    combatLoop player 1 (lEhp, lPhp) dataMap (enemy, inSeed)
            'e' ->  combatLoop player 9 (lEhp, lPhp) dataMap (enemy, inSeed)
            _ -> combatLoop player 1 (lEhp, lPhp) dataMap (enemy, inSeed)
    | phase == 2 = do   --Enemy attack
        let (takenDamage, outSeed) = enemyDamage enemy player inSeed
        eDamagePrint enemy takenDamage
        checkDeath player enemy 1 (lEhp, lPhp+takenDamage) dataMap outSeed
    | phase == 8 = do   --Free escape
        putStrLn escape
        pInput <- getLine
        if null pInput then combatLoop player 8 (lEhp, lPhp) dataMap (enemy, inSeed)
        else case toLower (head pInput) of
            'y' -> return (player, dataMap, inSeed, 0)
            'n' -> combatLoop player 1 (lEhp, lPhp) dataMap (enemy, inSeed)
            _ -> combatLoop player 8 (lEhp, lPhp) dataMap (enemy, inSeed)

    | phase == 9 = do   --Attempt escape
        let (coinFlip, outSeed) = randomR (0, 10) inSeed :: (Int, StdGen)
        if hp player-lPhp < eMaxHp enemy-lEhp && 3<=coinFlip then do
            putStrLn escapeFail
            combatLoop player 2 (lEhp, lPhp) dataMap (enemy, outSeed)
        else do
            putStrLn escapeSuccess
            let escapePlayer = updateHp player lPhp
            return (escapePlayer, dataMap, outSeed, 0)
    | phase == 10 = do  --Quit loop, player death
        printHp (name player) (eName enemy) (maxHp player-lPhp) (eMaxHp enemy-lEhp)
        return (player, dataMap, inSeed, 1)
    | phase == 11 = do  --Quit loop, enemy killed
        printHp (name player) (eName enemy) (maxHp player-lPhp) 0
        putStrLn (name player++killedEnemy1++eName enemy++killedEnemy2++show (eDrops enemy)++killedEnemy3++show (expDrop enemy)++killedEnemy4)
        return (updateHp (updateMoney (incrementExp player (expDrop enemy))  (eDrops enemy)) (-lPhp), dataMap, inSeed, 0)
    | otherwise = return (player, dataMap, inSeed, -1)-- Exit due to error

printHp :: String -> String -> Int -> Int  -> IO ()
printHp name1 name2 hp1 hp2 = do
    putStr "+"
    printLine 47
    putStr "| "
    putStr (name1++": ")
    printOneUsercontent " " (20-length name1)
    putStr (name2++": ")
    printOneUsercontent " " (20-length name2)
    putStrLn " "
    putStr "| "
    printOneUsercontent "|" hp1
    printOneUsercontent " " (20-hp1)
    printOneUsercontent "|" hp2
    printOneUsercontent " " (20-hp2)
    putStrLn " "
    putStr "+"
    printLine 47


printLine :: Int -> IO ()
printLine 0 = putStrLn "+"
printLine count = do
    putStr "-"
    printLine (count-1)


printOneUsercontent :: String -> Int -> IO ()
printOneUsercontent _ 0 = putStr "| "
printOneUsercontent symbol count = do
    if count < 0 then putStr "| "
    else do
        putStr symbol
        printOneUsercontent symbol (count-1)


pDamagePrint :: Player -> Int -> IO ()
pDamagePrint player damage = putStrLn (name player++" attacks, and deals "++show damage++"!")


eDamagePrint :: Enemy -> Int -> IO ()
eDamagePrint enemy damage = putStrLn("The "++eName enemy++" attacks, and deals "++show damage++"!")


--Redirects to next combat stage or to correct death stage if someone died
checkDeath :: Player -> Enemy -> Int -> (Int,Int) -> [[Int]] -> StdGen -> IO (Player, [[Int]], StdGen, Int)
checkDeath player enemy nextPhase (lEhp, lPhp) dataMap passSeed
    | hp player - lPhp <= 0 = combatLoop player 10 (lEhp, lPhp) dataMap (enemy, passSeed)
    | eMaxHp enemy - lEhp <= 0 = combatLoop player 11 (lEhp, lPhp) dataMap (enemy, passSeed)
    | otherwise = combatLoop player nextPhase (lEhp, lPhp) dataMap (enemy, passSeed)