module PlayerUpdate_tests(updateTests) where
import Test.QuickCheck
import Control.Exception (evaluate)
import Public.Consts.Structs (Player (..))
import Public.P_updatePlayer (levelUp, incrementExp, newLayer, increaseMaxHp, updateHp, updateWeapon, updateArmour, updateHeal, updateMoney, updateLayer, updatePos, updateGoal, updatePrevdir)
import Test.HUnit

playerTest = Player {name = "Tormod",maxHp=50, hp=10, weapon  = 3, armour  = 2, healpot = 3, money   = 10, lowestLayer = 0, playerPos = (0,0), start = (0,0), goal = (0,0), prevDir = 0, pExp = 0, levelCap = 1}
playerTestResult1 = Player {name = "Tormod",maxHp=60, hp=10, weapon  = 3, armour  = 2, healpot = 3, money   = 10, lowestLayer = 0, playerPos = (0,0), start = (0,0), goal = (0,0), prevDir = 0, pExp = 0, levelCap = 1}
playerTestResult2 = Player {name = "Tormod",maxHp=50, hp=20, weapon  = 3, armour  = 2, healpot = 3, money   = 10, lowestLayer = 0, playerPos = (0,0), start = (0,0), goal = (0,0), prevDir = 0, pExp = 0, levelCap = 1}
playerTestResult3 = Player {name = "Tormod",maxHp=50, hp=10, weapon  = 8, armour  = 2, healpot = 3, money   = 10, lowestLayer = 0, playerPos = (0,0), start = (0,0), goal = (0,0), prevDir = 0, pExp = 0, levelCap = 1}
playerTestResult4 = Player {name = "Tormod",maxHp=50, hp=10, weapon  = 3, armour  = 12, healpot = 3, money   = 10, lowestLayer = 0, playerPos = (0,0), start = (0,0), goal = (0,0), prevDir = 0, pExp = 0, levelCap = 1}
playerTestResult5 = Player {name = "Tormod",maxHp=50, hp=10, weapon  = 3, armour  = 2, healpot = 13, money   = 10, lowestLayer = 0, playerPos = (0,0), start = (0,0), goal = (0,0), prevDir = 0, pExp = 0, levelCap = 1}
playerTestResult6 = Player {name = "Tormod",maxHp=50, hp=10, weapon  = 3, armour  = 2, healpot = 3, money   = 20, lowestLayer = 0, playerPos = (0,0), start = (0,0), goal = (0,0), prevDir = 0, pExp = 0, levelCap = 1}
playerTestResult7 = Player {name = "Tormod",maxHp=50, hp=10, weapon  = 3, armour  = 2, healpot = 3, money   = 10, lowestLayer = 1, playerPos = (0,0), start = (0,0), goal = (0,0), prevDir = 0, pExp = 0, levelCap = 1}
playerTestResult8 = Player {name = "Tormod",maxHp=50, hp=10, weapon  = 3, armour  = 2, healpot = 3, money   = 10, lowestLayer = 0, playerPos = (1,1), start = (0,0), goal = (0,0), prevDir = 0, pExp = 0, levelCap = 1}
playerTestResult9 = Player {name = "Tormod",maxHp=50, hp=10, weapon  = 3, armour  = 2, healpot = 3, money   = 10, lowestLayer = 0, playerPos = (0,0), start = (0,0), goal = (2,2), prevDir = 0, pExp = 0, levelCap = 1}
playerTestResult10 = Player {name = "Tormod",maxHp=50, hp=10, weapon  = 3, armour  = 2, healpot = 3, money   = 10, lowestLayer = 0, playerPos = (0,0), start = (0,0), goal = (0,0), prevDir = 3, pExp = 0, levelCap = 1}
playerTestResult11 = Player {name = "Tormod",maxHp=51, hp=11, weapon  = 3, armour  = 2, healpot = 3, money   = 10, lowestLayer = 1, playerPos = (1,1), start = (1,1), goal = (2,2), prevDir = 0, pExp = 0, levelCap = 1}
playerTestResult12 = Player {name = "Tormod",maxHp=50, hp=10, weapon  = 3, armour  = 2, healpot = 3, money   = 10, lowestLayer = 0, playerPos = (0,0), start = (0,0), goal = (0,0), prevDir = 0, pExp = 2, levelCap = 1}
playerTestResult13 = Player {name = "Tormod",maxHp=52, hp=12, weapon  = 3, armour  = 3, healpot = 3, money   = 10, lowestLayer = 0, playerPos = (0,0), start = (0,0), goal = (0,0), prevDir = 0, pExp = -1, levelCap = 2}

updateTests :: Test
updateTests = TestList [
  TestLabel "increaseMaxHp" $
    TestCase $ assertEqual "increases max HP of player"
    (increaseMaxHp playerTest 10)
    playerTestResult1,

  TestLabel "updateHp" $
    TestCase $ assertEqual "updates player currentHP"
    (updateHp playerTest 10)
    playerTestResult2,

  TestLabel "updateWeapon" $
    TestCase $ assertEqual "increases player damage"
    (updateWeapon playerTest 5)
    playerTestResult3,

  TestLabel "updateArmour" $
    TestCase $ assertEqual "increases player armour"
    (updateArmour playerTest 10)
    playerTestResult4,

  TestLabel "updateHeal" $
    TestCase $ assertEqual "gives player hp pots"
    (updateHeal playerTest 10)
    playerTestResult5,

  TestLabel "updateMoney" $
    TestCase $ assertEqual "gives player money"
    (updateMoney playerTest 10)
    playerTestResult6,

  TestLabel "updateLayer" $
    TestCase $ assertEqual "increases current player lowestlayer value by one"
    (updateLayer playerTest)
    playerTestResult7,

  TestLabel "updatePos" $
    TestCase $ assertEqual "overwrites player pos"
    (updatePos playerTest 1 1)
    playerTestResult8,

  TestLabel "updateGoal" $
    TestCase $ assertEqual "updates player goal"
    (updateGoal playerTest 2 2)
    playerTestResult9,

  TestLabel "updatePrevdir" $
    TestCase $ assertEqual "updates the previous direction the player went"
    (updatePrevdir playerTest 3)
    playerTestResult10,

  TestLabel "newLayer" $
    TestCase $ assertEqual "grants various buffs for new layer player"
    (newLayer playerTest (1,1) (2,2))
    playerTestResult11,

  TestLabel "incrementExp" $
    TestCase $ assertEqual "gives the player exp"
    (incrementExp playerTest 2)
    playerTestResult12,

  TestLabel "levelUp" $
    TestCase $ assertEqual "levels the player up"
    (levelUp playerTest (0,1))
    playerTestResult13
  ]