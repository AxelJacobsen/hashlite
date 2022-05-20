module Initialization.Impure.Ip_initChar (genNewFile) where

--generates a new file with given filename
genNewFile :: String -> String -> IO()
genNewFile = writeFile