module Ip_mainLoop (gameLoop) where

import Structs (Player)
import System.Random ( Random(randomR), StdGen )
-- Main Loop
gameLoop :: Player -> ([[Int]], StdGen) -> IO ()
gameLoop _ (_,_) = print  "Cookie for you :)"
    -- PRINT CURRENT MAP
    -- PRINT "VISUAL MOVEMENT OPTIONS"
    -- PRINT INVENTORY OPTIONS