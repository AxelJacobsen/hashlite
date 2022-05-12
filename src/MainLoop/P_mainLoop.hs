module MainLoop.P_mainLoop(checkLegalIdleChoice) where
import Data.Char (toLower)

checkLegalIdleChoice :: Char -> []Char -> Int -> Int
checkLegalIdleChoice entry [] pos = 0
checkLegalIdleChoice entry (option:options) count
    | entry == option = count+1
    | otherwise = checkLegalIdleChoice entry options (count+1)