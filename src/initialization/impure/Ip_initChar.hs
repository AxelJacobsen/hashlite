module Initialization.Impure.Ip_initChar (genNewFile) where
import TextGeneral
import Structs (Player (..))
import System.Directory (doesFileExist)
import Initialization.Pure.P_initChar (generateCharacter)
import Consts

--generates a new file with given filename
genNewFile :: String -> String -> IO()
genNewFile = writeFile