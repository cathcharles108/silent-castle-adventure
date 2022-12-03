module Example where

import System.Random
import Data.List
import qualified Data.Map as M

import Item
import Direction
import Room
import Player
import GameState

-- assignment 3 3.2.1 choose copied from Lab
--choose :: [a] -> IO a
--choose lst = do
--  index <- randomRIO (0, length lst - 1)
--  return (lst !! index)
--
---- assignment 3 3.2.2
--exampleList :: IO a -> IO Int -> IO [a]
--exampleList x len = do
--  l <- len
--  sequence $ replicate l x
--
---- assignment 3 3.2.6
--exitExample :: IO Exit
--exitExample = do
--  dir <- example :: IO Direction
--  rm <- choose roomNames
--  return (dir, rm)
--
---- assignment 3 3.2.3 Example type class creation
--class Example a where
--  example :: IO a
--
---- assignment 3 3.2.4
--instance Example Item where
--  example = do
--    name <- (choose itemNames)
--    heavy <- (randomRIO (0,100))
--    return (Item name heavy)
--
---- assignment 3 3.2.5
--instance Example Direction where
--  example = choose [N,S,E,W]
--
---- assignment 3 3.2.7
--instance Example Room where
--  example = do
--    rm <- choose roomNames
--    let des = "You are in a randomly-generated room, the " ++ (show rm) ++ "."
--    randExit <- (exampleList exitExample (randomRIO(2,4)))
--    randObj <- (exampleList (choose itemNames) (randomRIO(2,5)))
--    return (Room rm des randExit randObj)
--
---- assignment 3 3.2.8
--instance Example Player where
--  example = do
--    -- stuff :: IO [Item]
--    dupstuff <- (exampleList (choose itemList) (randomRIO(0,10)))
--    let stuff = nub dupstuff
--    -- take out names of the stuff loaded
--    let stuffNames = map iname stuff
--    -- take out weights of the stuff loaded
--    let weights = map weight stuff
--    let heavy = (if weights == []
--                then 0
--                else (maximum weights) + (minimum weights))
--    rm <- choose roomNames
--    return (Player stuffNames heavy rm)
--
---- assignment 4 3.1.1
--instance Example GameState where
--  example = do
--    msg <- choose [ Just "One possible message."
--                  , Just "Yet another possible message"
--                  , Nothing]
--    rms <- exampleList (example :: IO Room) (randomRIO(2,3))
--    let rooms = mkMap rms
--    stuff <- exampleList (example :: IO Item) (randomRIO(5,10))
--    let things = mkUniverse stuff
--    person <- example :: IO Player
--    return (GameState msg rooms things person)