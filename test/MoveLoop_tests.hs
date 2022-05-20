module MoveLoop_tests(moveTests) where
import Test.QuickCheck
import Control.Exception (evaluate)
import System.Random (  StdGen )
import MoveLoop.P_Move(checkForLegalMove, checkTileValue)
import Test.HUnit

moveTests :: Test
moveTests = TestList [
  TestLabel "checkForLegalMove" $
    TestCase $ assertEqual "checks if desired coords are on the map, and not start"
    (checkForLegalMove (1,1) 99 [[0,0,0],[0,10,0],[0,0,0]])
    ([[0,0,0],[0,99,0],[0,0,0]], True),

  TestLabel "checkTileValue" $
    TestCase $ assertEqual "checks the actual tile value"
    (checkTileValue (1,1) [[0,0,0],[0,10,0],[0,0,0]])
    10
  ]