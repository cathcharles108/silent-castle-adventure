module Player where

import Data.List (delete)

import Item
import Room
import Tasks

data Player
  = Player { inventory :: [ItemName]
           , maxWeight :: Integer
           , location :: RoomName
           , todo :: [TaskName]}
  deriving (Show, Eq)

-- adding an item to a player's inventory
addItem :: ItemName -> Player -> Player
addItem thing person = Player (thing : inventory person)
                       (maxWeight person)
                       (location person)
                       (todo person)

-- removing an item from a player's inventory
removeItem :: ItemName -> Player -> Player
removeItem thing person = Player (delete thing (inventory person))
                          (maxWeight person)
                          (location person)
                          (todo person)

-- add task to player's to-do list
addTask :: TaskName -> Player -> Player
addTask task person = Player (inventory person)
                             (maxWeight person)
                             (location person)
                             (task : todo person)

-- remove task from player's to-do list
removeTask :: TaskName -> Player -> Player
removeTask task person = Player (inventory person)
                                (maxWeight person)
                                (location person)
                                (delete task (todo person))

-- change the location of the player
newLocation :: RoomName -> Player -> Player
newLocation place person = person{ location = place }

-- bool function whether the player is carrying things
isCarryingAnything :: Player -> Bool
isCarryingAnything person = inventory person /= []

-- defined as the game player
you :: Player
you = Player [] 100 GreatHall [GoToTower]