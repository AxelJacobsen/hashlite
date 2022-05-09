module MainLoop.P_mainLoop(checkLegalIdleChoice,getListValue) where
import Data.Char (toLower)

checkLegalIdleChoice :: Char -> []Char -> Int -> Int
checkLegalIdleChoice entry [] pos = -1
checkLegalIdleChoice entry (option:options) count
    | entry == option = count+2
    | otherwise = checkLegalIdleChoice entry options (count+1)

getListValue :: []Int -> Int -> Int
getListValue [] _ = -99
getListValue (value:listOfValues) 0 = value
getListValue (value:listOfValues) num = getListValue listOfValues (num-1)