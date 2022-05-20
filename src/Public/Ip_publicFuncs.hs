module Public.Ip_publicFuncs(actionHandler,clearConsole, checkLevelUp)where
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
                's' -> return (levelUp inPlayer (3, 0))
                'a' -> return (levelUp inPlayer (0, 3))
                _ -> checkLevelUp inPlayer
        else checkLevelUp inPlayer
    else return inPlayer

actionHandler :: String -> IO Bool
actionHandler question = do
    putStrLn question
    yesNo <- getLine
    if not (null yesNo) then do
        case toLower (head yesNo) of
            'y' -> return True
            'n' -> return False
            _ -> actionHandler question
    else actionHandler question