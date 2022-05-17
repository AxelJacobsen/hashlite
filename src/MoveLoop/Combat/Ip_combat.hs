module MoveLoop.Combat.Ip_combat where
import Structs (Player (..), Enemy(..))
import System.Random ( Random(randomR), StdGen )
import System.IO ( hGetContents, openFile, IOMode(ReadMode) )
import MoveLoop.Combat.CombatText (encounterEnemy, coinSuccess, coinNeutral, coinCritical, escape, pAttack1, pAttack2, eAttack, damage)
import Public.P_updatePlayer (updatePos, newLayer)

-- I want this to go right into the loop to skip unecessary nests
-- therefore creature type has to be predecided
combatLoop :: Player -> Int -> (Int,Int) -> [[Int]] -> (Enemy, StdGen) -> IO (Player, [[Int]], StdGen)
combatLoop player phase (lEhp, lPhp) unexploredMap (enemy, inSeed) -- lehp and phph are temporary damage counters to reduce function calls
    | phase == 0 = do-- Enter comabt
        let (coinFlip, outSeed) = randomR (0, 6) inSeed :: (Int, StdGen)
        case coinFlip of
            0 -> do
                putStrLn (coinCritical++prefix enemy++eName enemy++"!")
                combatLoop player 2 (lEhp, lPhp) unexploredMap (enemy, inSeed)
            6 -> do
                putStrLn (encounterEnemy++prefix enemy++eName enemy)
                putStrLn (coinSuccess++escape)
                combatLoop player 3 (lEhp, lPhp) unexploredMap (enemy, inSeed)
            _ -> do
                putStrLn (encounterEnemy++prefix enemy++eName enemy)
                putStrLn coinNeutral
                combatLoop player 1 (lEhp, lPhp) unexploredMap (enemy, inSeed)
    | phase == 1 = do       -- Player Turn
        printHp (name player) (eName enemy) (maxHp player-lPhp) (eMaxHp enemy-lEhp)
        combatLoop player 2 (lEhp, lPhp) unexploredMap (enemy, inSeed)
    | phase == 2 = do   --Enemy attack
        printHp (name player) (eName enemy) (maxHp player-lPhp) (eMaxHp enemy-lEhp)
        combatLoop player 3 (lEhp, lPhp) unexploredMap (enemy, inSeed)
    | otherwise = return (player, unexploredMap, inSeed)-- Exit due to error


printHp :: String -> String -> Int -> Int  -> IO ()
printHp name1 name2 hp1 hp2 = do
    putStr "+"
    printLine 44
    putStr "| "
    putStr (name1++": ")
    printOneUsercontent " " (20-length name1)
    putStr (name2++": ")
    printOneUsercontent " " (20-length name2)
    printOneUsercontent "|" hp1
    printOneUsercontent " " (22-hp1)
    putStrLn " "
    printOneUsercontent "|" hp2
    printOneUsercontent " " (22-hp2)
    putStrLn " "
    putStr "+"
    printLine 44

printLine :: Int -> IO ()
printLine 0 = putStrLn "+"
printLine count = do
    putStr "-"
    printLine (count-1)

printOneUsercontent :: String -> Int -> IO ()
printOneUsercontent _ 0 = putStr "|"
printOneUsercontent symbol count = do
    putStr symbol
    printOneUsercontent symbol (count-1)