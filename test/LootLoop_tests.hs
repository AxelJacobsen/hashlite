module LootLoop_tests(lootTest) where
import Test.QuickCheck
import Control.Exception (evaluate)
import Public.Consts.Structs (Player (..))
import System.Random ( Random(randomR), StdGen, mkStdGen )
import MoveLoop.Loot.P_loot(handleTrap, givePlayerLoot)
import Test.HUnit

testSeed = mkStdGen 10

inPlayer =  Player {name = "Tormod", maxHp=8, hp=8, weapon  = 3, armour  = 2, healpot = 3, money   = 10, lowestLayer = 0, playerPos = (0,0), start = (0,0), goal = (0,0), prevDir = 0, pExp = 0, levelCap = 1}
trappedPlayer = Player {name = "Tormod", maxHp=8, hp=7, weapon  = 3, armour  = 2, healpot = 3, money   = 10, lowestLayer = 0, playerPos = (0,0), start = (0,0), goal = (0,0), prevDir = 0, pExp = 0, levelCap = 1}
lootPlayer = Player {name = "Tormod", maxHp=8, hp=8, weapon  = 3, armour  = 5, healpot = 3, money   = 10, lowestLayer = 0, playerPos = (0,0), start = (0,0), goal = (0,0), prevDir = 0, pExp = 0, levelCap = 1}


lootTest :: Test
lootTest = TestList [
  TestLabel "handleTrap" $
    TestCase $ assertEqual "deals damage from trap to player"
    (handleTrap inPlayer)
    trappedPlayer,

  TestLabel "givePlayerLoot" $
    TestCase $ assertEqual "gives player loot"
    (givePlayerLoot inPlayer 1)
    lootPlayer
  ]
