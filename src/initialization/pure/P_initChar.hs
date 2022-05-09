module Initialization.Pure.P_initChar (generateCharacter) where
import Structs (Player (..))

generateCharacter :: String -> Player
generateCharacter name = Player { 
    name = name,
    hp=10, 
    weapon  = 0, 
    armour  = 0, 
    healpot = 1, 
    money   = 10, 
    lowestLayer = 0}