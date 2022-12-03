module Main where

import Test.Tasty
import Test.Tasty.HUnit

-- 3.9.6 : replace this whole module with your own unit test suite
import Item
import Direction
import Room
import Player
import GameState
import Example

isCarryingAnythingTest :: TestTree
isCarryingAnythingTest =
  let p = Player [] 75 GreatHall []
  in testGroup
     "isCarryingAnything tests"
     [ testCase "REPL example 1 works"
       (isCarryingAnything p @?= False)
     , testCase "REPL example 2 works"
       (isCarryingAnything (Player.addItem Goblet p) @?= True)
     ]

tests :: TestTree
tests = testGroup "Assignment 2 Tests"
        [ testGroup "Project Tests" [ isCarryingAnythingTest ]]

main :: IO ()
main = defaultMain tests
