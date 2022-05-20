module Public.Ip_publicFuncs(clearConsole, checkLevelUp)where
import Public.Consts.Structs (Player (..))
import Public.Consts.TextGeneral(levelUp1,levelUp2)
import Data.Char (toLower)
import Public.P_updatePlayer(levelUp)
clearConsole :: IO ()
clearConsole = printLineRec 50

printLineRec :: Int -> IO ()
printLineRec 0 = putStrLn ""
printLineRec tim = do
    putStrLn ""
    printLineRec (tim-1)


checkLevelUp :: Player -> IO Player
checkLevelUp inPlayer = do
    if levelCap inPlayer<=pExp inPlayer then do
        putStrLn (levelUp1++name inPlayer++levelUp2)
        choice <- getLine
        if not (null choice) then do
            case toLower (head choice) of
                's' -> return (levelUp inPlayer (lowestLayer inPlayer, 0))
                'a' -> return (levelUp inPlayer (0, lowestLayer inPlayer))
                _ -> checkLevelUp inPlayer
        else checkLevelUp inPlayer
    else return inPlayer