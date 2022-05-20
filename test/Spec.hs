import Test.QuickCheck
import Control.Exception (evaluate)
import Public.Consts.Structs (Player (..))
import System.Random ( Random(randomR), StdGen )
import System.Directory(doesFileExist)
import MainLoop.P_mainLoop(checkLegalIdleChoice)
import Initialization.Pure.P_initChar (generateCharacter, placeStartEnd)
import Initialization.Pure.P_MapGenerator (generateBoard, generateEmptyBoard)
import MoveLoop.P_Move(checkForLegalMove)
import MoveLoop.Combat.P_combat(generateEnemy, genMimic)
import Public.P_updatePlayer (incrementExp, updatePos, newLayer)
import Public.P_publicFuncs (healPlayer)

import Initialization_tests(initTests)
import MainLoop_tests(mainTests)
import LootLoop_tests(lootTest)
import CombatLoop_tests(combatTests)
import MoveLoop_tests(moveTests)
import Test.HUnit (runTestTT)
import PublicFunc_tests(publicTests)
import PlayerUpdate_tests(updateTests)

main :: IO ()
main = do
    _ <- runTestTT initTests
    _ <- runTestTT mainTests
    _ <- runTestTT combatTests
    _ <- runTestTT lootTest
    _ <- runTestTT moveTests
    _ <- runTestTT publicTests
    _ <- runTestTT updateTests

    putStrLn "Passed all tests!"