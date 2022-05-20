module MainLoop_tests(mainTests) where
import Test.QuickCheck
import Control.Exception (evaluate)
import System.Random ( Random(randomR), StdGen )
import MainLoop.P_mainLoop(checkLegalIdleChoice)
import Test.HUnit

mainTests :: Test
mainTests = TestList [
  TestLabel "checkLegalIdleChoice" $
    TestCase $ assertEqual "checks if a character is in a list, then returns index+1"
    (checkLegalIdleChoice 'a' ['a','b','c'] 0)
    1
  ]