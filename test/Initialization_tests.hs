module Initialization_tests (initTests) where
import Test.QuickCheck
import Control.Exception (evaluate)
import Public.Consts.Structs (Player (..))
import System.Random ( Random(randomR), StdGen, mkStdGen )
import Initialization.Pure.P_initChar (calcLower, generateCharacter, placeStartEnd, setMarkerEntry,setMarkerInner)
import Initialization.Pure.P_MapGenerator(fillInnerBoardEmpty', fillBoardEmpty', generateEmptyBoard)
import Test.HUnit

testSeed = mkStdGen 10

playerTest1 = Player {name = "Tormod",maxHp=8, hp=8, weapon  = 3, armour  = 2, healpot = 3, money   = 10, lowestLayer = 0, playerPos = (0,0), start = (0,0), goal = (0,0), prevDir = 0, pExp = 0, levelCap = 1}
playerTest2 = Player {name = "Jostein",maxHp=8, hp=8, weapon  = 3, armour  = 2, healpot = 3, money   = 10, lowestLayer = 0, playerPos = (0,0), start = (0,0), goal = (0,0), prevDir = 0, pExp = 0, levelCap = 1}

startEndTestMap1 = [[0,0,0],[0,0,0],[0,0,0]]
startEndResultMap1 = [[0,99,0],[0,0,0],[0,0,0]]
setMarkEntryResultMap = [[0,0,0],[0,0,0],[0,99,0]]
serMarkInnerResultMap = [0,99,0]

initTests :: Test
initTests = TestList [
  TestLabel "generateCharacter" $
    TestCase $ assertEqual "creates a new character with given name"
    (generateCharacter "Tormod")
    playerTest1,
    TestCase $ assertEqual "creates character with empty name"
    (generateCharacter "")
    playerTest2,

  TestLabel "placeStartEnd" $
    TestCase $ assertEqual "places Start and end randomly on a map"
    (placeStartEnd (length startEndTestMap1) True (startEndTestMap1, testSeed))
    (startEndResultMap1,testSeed,2,1),

  TestLabel "setMarkerEntry" $
    TestCase $ assertEqual "places a marker in a spot on a 2d map"
    (setMarkerEntry (1,1) (length setMarkEntryResultMap, length setMarkEntryResultMap) 99 startEndTestMap1 [[]])
    setMarkEntryResultMap,

  TestLabel "setMarkerInner" $
    TestCase $ assertEqual "places a marker in a spot on a 1d list"
    (setMarkerInner 1 (length serMarkInnerResultMap) 99 [] (head startEndTestMap1))
    serMarkInnerResultMap,

  TestLabel "calcLower" $
    TestCase $ assertEqual "Gets lower rng limit for startPos"
    (calcLower 10 0.7)
    7,

  --Map Generation tests below
  TestLabel "generateEmptyBoard" $
    TestCase $ assertEqual "generates an 2d map filled with (-1)"
    (generateEmptyBoard 3)
    [[-1,-1,-1],[-1,-1,-1],[-1,-1,-1]],

  TestLabel "fillBoardEmpty'" $
    TestCase $ assertEqual "generates an 2d map filled with (-1), handles y iteration"
    (fillBoardEmpty' 3 3 [[]])
    [[-1,-1,-1],[-1,-1,-1],[-1,-1,-1]],

  TestLabel "fillInnerBoardEmpty'" $
    TestCase $ assertEqual "fills a list with -1"
    (fillInnerBoardEmpty' 3 [])
    [-1,-1,-1]

  ]