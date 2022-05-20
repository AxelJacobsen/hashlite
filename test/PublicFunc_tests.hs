module PublicFunc_tests(publicTests) where
import Test.QuickCheck
import Public.Consts.Structs(Player(..))
import Control.Exception (evaluate)
import Public.P_publicFuncs (drop', healPlayer)
import Test.HUnit

playerTest = Player {name = "Tormod",maxHp=50, hp=10, weapon  = 3, armour  = 2, healpot = 3, money   = 10, lowestLayer = 0, playerPos = (0,0), start = (0,0), goal = (0,0), prevDir = 0, pExp = 0, levelCap = 1}
playerTestResult = Player {name = "Tormod",maxHp=50, hp=20, weapon  = 3, armour  = 2, healpot = 3, money   = 10, lowestLayer = 0, playerPos = (0,0), start = (0,0), goal = (0,0), prevDir = 0, pExp = 0, levelCap = 1}

publicTests :: Test
publicTests = TestList [
  TestLabel "drop'" $
    TestCase $ assertEqual "drops the first x values of a list"
    (drop' 3 [1,2,3,4,5])
    [4,5],

  TestLabel "healPlayer" $
    TestCase $ assertEqual "heals players for aportion of their health"
    (healPlayer (playerTest, 0))
    (playerTestResult, -10)
  ]