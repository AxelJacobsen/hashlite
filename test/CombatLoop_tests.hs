module CombatLoop_tests(combatTests) where
import Test.QuickCheck
import Control.Exception (evaluate)
import Public.Consts.Structs(Enemy(..))
import System.Random ( StdGen, mkStdGen)
import MoveLoop.Combat.P_combat(genMimic, generateEnemyInner, legalizeDamage)
import Test.HUnit

testSeed = mkStdGen 10

generatedMimic = Enemy {prefix = "a ", eName = "Mimic", eMaxHp = 12, eDamage = 8, eArmour = 32, eDrops = 20, expDrop = 1}
generatedOdd = Enemy{prefix = "", eName = "Oddvar Braa", eMaxHp = 1200, eDamage = 160, eArmour = 30, eDrops = 2766342, expDrop = 1000}

combatTests :: Test
combatTests = TestList [
  TestLabel "genMimic" $
    TestCase $ assertEqual "generates a Mimic type enemy"
    (genMimic 2)
    generatedMimic,

  TestLabel "generateEnemyInner" $
    TestCase $ assertEqual "generates an enemy based on random parameters"
    (generateEnemyInner 0 2 100)
    generatedOdd,

  TestLabel "legalizeDamage" $
    TestCase $ assertEqual "Ensures that damage cant be less than one"
    (legalizeDamage (0,testSeed, False))
    (1,testSeed,False)
  ]
