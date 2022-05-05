module Ip_initChar (initChar) where
import TextGeneral (nameCharacter)
import Structs (Player (..))
import System.Directory (doesFileExist)

initChar :: String -> String -> Player
initChar "" _ = generateCharacter
initChar namePath dataPath = handlePlayerData dataPath $ handleName namePath

handlePlayerData :: String -> String -> Player
handlePlayerData "" charName = generateCharacter "" charName
handlePlayerData fileName charName
    | fileName == "" = print "INTERNAL ERROR, MISSING CHARACTER DATA FILEPATH"
    | charName == "" = handlePlayerData fileName getNewName
    | not (doesFileExist fileName) = do
        genNewFile fileName
        getNewName
    | otherwise = do
        let charData = content $ readFile namePath
        if charData == "" then generateCharacter "" charName
        else generateCharacter charData charName

handleName :: String -> IO String
handleName fileName
    | fileName == "" = print "INTERNAL ERROR, MISSING CHARACTER NAME FILEPATH"
    | not (doesFileExist fileName) = do
        genNewFile fileName
        getLine
    | otherwise = do
        let charName = content $ readFile namePath
        if charName == "" then getLine
        else charName

genNewFile :: String -> IO()
genNewFile fileName = writeFile fileName ""