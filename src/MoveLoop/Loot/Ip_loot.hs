module MoveLoop.Loot.Ip_loot(lootLoop) where
import MoveLoop.Loot.LootText(discover,openChest,trappedChest,trappedDeath,trappedDamage,mimicChest,lootChest,lootItems)
import MoveLoop.Loot.P_loot(handleTrap,givePlayerLoot)
import Public.Consts.Structs (Player (..), Enemy(..))
import System.Random ( Random(randomR), StdGen )
import System.IO ( hGetContents, openFile, IOMode(ReadMode) )
import MoveLoop.Combat.CombatText (missedAttack, killedEnemy1,killedEnemy2,killedEnemy3,killedEnemy4, encounterEnemy, coinSuccess, coinNeutral, coinCritical, escape, pAttack1, pAttack2, eAttack, damage, escapeFail, escapeSuccess, combatOptions1, combatOptions2)
import Public.Consts.TextGeneral(healMessage,outOfHeal)
import Public.P_updatePlayer (updatePos, newLayer, updateHp, updateHeal,updateMoney, incrementExp, updateArmour, updateWeapon)
import Data.Char (toLower)
import Public.P_publicFuncs(healPlayer)
import Public.Ip_publicFuncs(clearConsole)
import MoveLoop.Combat.P_combat(enemyDamage,playerDamage)
import MoveLoop.P_Move(checkForLegalMove)

lootLoop :: Player -> Int -> StdGen -> IO (Player, StdGen, Int)
lootLoop player phase inSeed
    | phase == 0 = do-- Ask if want to open chest, and display print
        putStrLn discover
        wantOpen <- getLine
        if not (null wantOpen) then do
            case toLower (head wantOpen) of
                'y' -> lootLoop player 1 inSeed
                'n' -> return (player, inSeed, 0)
                _ -> lootLoop player 0 inSeed
        else lootLoop player 0 inSeed
    
    | phase == 1 = do
        let mimic = 10
        let (chestType, nextSeed) = randomR (0, mimic) inSeed :: (Int, StdGen)
        putStr openChest
        case chestType of
            0 -> do                                                         --TRAPPED
                let burntPlayer = handleTrap player
                if hp burntPlayer <= 0 then do putStrLn trappedDeath ; return (burntPlayer, nextSeed, 1) 
                else do 
                    putStrLn (trappedDamage++show(hp player - hp burntPlayer)++" damage!")
                    return (burntPlayer, nextSeed, 0)
            mimic -> do putStrLn mimicChest ; return (player, nextSeed, 2)      --MIMIC
            _ -> lootLoop player 2 nextSeed                                 --SAFE CHEST
    
    | phase == 2 = do
        let (lootType, nextSeed) = randomR (0, length lootItems -1) inSeed :: (Int, StdGen)
        putStrLn (lootChest++(lootItems!!lootType))                         --Prints loot content
        return (givePlayerLoot player lootType, inSeed, 0)                  --Returns player with loot
    | otherwise = return (player, inSeed, -99)-- Exit due to error
