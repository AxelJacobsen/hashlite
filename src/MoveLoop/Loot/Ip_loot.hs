module MoveLoop.Loot.Ip_loot(garb) where

import MainLoop.Ip_mainLoop (gameLoop)

-- Since this isnt a loop all logic impure logic can just be done here
garb :: Int -> Int
garb c = c+1