module GameState where

import Data.List
import Control.Exception
import qualified Data.Map as M

import Item
import Room
import Player
import Direction
import Tasks

type GameMap = M.Map RoomName Room
type Error a = Either String a

data GameState
  = GameState { message :: Maybe String
              , gmap :: GameMap
              , universe :: Universe
              , player :: Player
              , alreadyDone :: [TaskName]
              , hiddenTasks :: [TaskName]}
  deriving (Show, Eq)

-- assignment 4 1.7.2
-- turns a list of rooms into the map of our game
mkMap :: [Room] -> GameMap
--mkMap rooms =
--  let mkMap' [] = []
--      mkMap' (x : xs) = [(rname x, x)] ++ mkMap' xs
--  in M.fromList (mkMap' rooms)
mkMap rooms =
    M.fromList (map (\x -> (rname x, x)) rooms)

-- assignment 4 3.2.2
gameMap :: GameMap
gameMap = mkMap allRooms

-- assignment 4 3.2.4
initialState :: GameState
initialState = GameState Nothing gameMap univ you [] allTasks

-- assignment 4 3.3
data KeyError = KeyError
  deriving Show

instance Exception KeyError

getObjectUniv :: ItemName -> Universe -> Item
getObjectUniv iname u
  = case M.lookup iname u of
      Just obj -> obj
      Nothing -> throw KeyError

getObject :: ItemName -> GameState -> Item
getObject iname st = getObjectUniv iname (universe st)

getRoomMap :: RoomName -> GameMap -> Room
getRoomMap rname mp
  = case M.lookup rname mp of
      Just room -> room
      Nothing -> throw KeyError

getRoom :: RoomName -> GameState -> Room
getRoom name st = getRoomMap name (gmap st)

-- swap out a room for a replacement room in a list of rooms
updateRoom :: RoomName -> Room -> [Room] -> [Room]
updateRoom _ _ [] = []
updateRoom roomName room (x : xs) =
  if ((rname x) == roomName)
  then (room : (updateRoom roomName room xs))
  else (x : (updateRoom roomName room xs))

-- replace the game map of a current game state
changeMap :: GameMap -> GameState -> GameState
changeMap gm st = st{ gmap = gm }
--changeMap gm st = GameState (message st)
--                            gm
--                            (universe st)
--                            (player st)
--                            (alreadyDone st)
--                            (hiddenTasks st)

-- assignment 4 3.3.1
-- updates a Game Map with an updated room
-- not for use to move the player around, just to update rooms
-- use like this: setRoomMap GreatHall (openExit (currentRoom st))
--                                      (gmap st)
setRoomMap :: RoomName -> Room -> GameMap -> GameMap
setRoomMap roomName room mp = mkMap (updateRoom roomName room (M.elems mp))

-- changes the current state of the great hall to one that has an exit out
-- use like this: openExit GreatHall (currentRoom st)
openExit :: Room -> Room
openExit rm =
  case (rname rm) of
    GreatHall -> (addExit (W,RoyalGarden) rm)
    _ -> rm

-- add an exit to any room
addExit :: Exit -> Room -> Room
addExit newExit rm = Room (rname rm)
                          (desc rm)
                          ((exits rm) ++ [newExit])
                          (objects rm)
--  case (destinationName dir rm) of
--    Nothing -> Room (rname rm)
--                    (desc rm)
--                    ((exits rm) ++ [(dir, rmname)])
--                    (objects rm)
--    Just _ -> rm

-- assignment 4 3.3.2
setMessage :: String -> GameState -> GameState
setMessage msg st =
    st{ message = if msg == "" then Nothing else Just msg }
--setMessage msg st =
--  if msg == ""
--  then GameState Nothing
--                 (gmap st)
--                 (universe st)
--                 (player st)
--                 (alreadyDone st)
--                 (hiddenTasks st)
--  else GameState (Just msg)
--                 (gmap st)
--                 (universe st)
--                 (player st)
--                 (alreadyDone st)
--                 (hiddenTasks st)

-- assignment 4 3.3.3
currentInventory :: GameState -> [ItemName]
currentInventory st = inventory (player st)

currentTodoList :: GameState -> [TaskName]
currentTodoList st = todo (player st)

-- assignment 4 3.3.4
currentRoom :: GameState -> Room
currentRoom st =
  let roomName = location (player st)
  in getRoom roomName st

-- assignment 4 3.3.5
-- things in the current room of the player
nearbyObjects :: GameState -> [ItemName]
nearbyObjects st =
  let room = currentRoom st
  in (objects room)

-- monadic and clean version
takeItemHelper :: ItemName -> GameState -> Error GameState
takeItemHelper thing st =
  alreadyHaveTakeCheck thing st
    >>= inRoomTakeCheck thing
    >>= weightCheck thing

-- add custom message when pick up scroll and key
takeItemMessage :: ItemName -> GameState -> String
takeItemMessage thing st =
  case thing of
    Scroll -> ("You take the scroll. It's a letter! " ++
              "You put the scroll in your pocket.\n" ++
              "Type 'scroll' at any point in the game to read it.")
    Tome ->
      case (TakeBook `elem` alreadyDone st) of
        True -> "You take the tome."
        False -> "You take the tome. A key fell out!"
    Key ->
      case (TakeKey `elem` alreadyDone st) of
        True -> "You take the key."
        False -> "You take the key. It looks like the key to your chest."
    _ -> "You take the " ++ (show thing) ++ "."

-- assignment 4 3.3.6
-- player picks up item from room and adds to inventory
takeItem :: ItemName -> GameState -> GameState
takeItem thing st =
  let curr_room = currentRoom st
      new_room = Room.removeItem thing curr_room
      new_map = setRoomMap (rname curr_room) new_room (gmap st)
      s = takeItemMessage thing st
  in case (takeItemHelper thing st) of
    Left msg -> (setMessage msg st)
    Right st -> (setMessage s (GameState (message st)
                              (new_map)
                              (universe st)
                              (Player.addItem thing (player st))
                              (alreadyDone st)
                              (hiddenTasks st)))

dropItemHelper :: ItemName -> GameState -> Error GameState
dropItemHelper thing st =
  anywhereDropCheck thing st
    >>= inRoomDropCheck thing

-- add custom message when use key to unlock chest
dropItemMessage :: ItemName -> Room -> String
dropItemMessage thing rm =
  let criteria = (rname rm == RoyalBedroom) && (Chest `elem` (objects rm))
  in case ((thing == Key) && criteria) of
    True -> ("You use the key on the chest. " ++
             "The magic paintbrush is inside!\n" ++
             "You put the key in your pocket.")
    False -> "You drop the " ++ (show thing) ++ "."

-- assignment 4 3.3.7
-- player returns item to room
-- complements takeItem
dropItem :: ItemName -> GameState -> GameState
dropItem thing st =
  let curr_room = currentRoom st
      new_room = Room.addItem thing curr_room
      new_map = setRoomMap (rname curr_room) new_room (gmap st)
      s = dropItemMessage thing curr_room
  in case (dropItemHelper thing st) of
    Left msg -> (setMessage msg st)
    Right st -> setMessage s (GameState (message st)
                             (new_map)
                             (universe st)
                             (Player.removeItem thing (player st))
                             (alreadyDone st)
                             (hiddenTasks st))

-- check if player is carrying the magic paintbrush
isCarryingPaintbrush :: GameState -> Error GameState
isCarryingPaintbrush st =
  let stuff = currentInventory st
  in case (Paintbrush `elem` stuff) of
    False -> Left ("You don't have your magic paintbrush with you.")
    True -> Right st

foundPaintbrush :: GameState -> Bool
foundPaintbrush st = FindPaintbrush `elem` (alreadyDone st)

hasFoundPaintbrush :: GameState -> Error GameState
hasFoundPaintbrush st =
  case (foundPaintbrush st) of
    False -> Left ("I don't understand that.")
    True -> Right st

-- check if player is in the Great Hall
isInGreatHall :: GameState -> Error GameState
isInGreatHall st =
  let rm = currentRoom st
  in case (rname rm) of
    GreatHall -> Right st
    _ -> Left ("The door leading to the royal garden is in the Great Hall, " ++
               "not here.")

-- check if there is already an exit in that direction
isThereExit :: Direction -> GameState -> Error GameState
isThereExit dir st =
  let rm = currentRoom st
  in case (destinationName dir rm) of
    Just _ -> Left ("There is already an exit there.")
    Nothing -> Right st

paintDoorHelper :: Direction -> GameState -> Error GameState
paintDoorHelper dir st =
  hasFoundPaintbrush st
    >>= isCarryingPaintbrush
    >>= isThereExit dir
    >>= isInGreatHall

-- actually paint the door
paintDoor :: Direction -> GameState -> GameState
paintDoor dir st =
  let curr_room = currentRoom st
      newName = rname curr_room
      newMap = setRoomMap newName (openExit curr_room) (gmap st)
      s = ("You paint the west wall of the Great Hall to reveal the massive " ++
          "stained-glass door leading to the royal garden!\n" ++
          "Having been used, your paintbrush dissolves into golden light.")
  in case (paintDoorHelper dir st) of
    Left msg -> (setMessage msg st)
    Right st -> (thingDisappears Paintbrush
                                 (setMessage s (changeMap newMap st)))

-- assignment 5 3.0.1
-- total weight of player's inventory
inventoryWeight :: GameState -> Integer
inventoryWeight st =
  let ls = inventory $ player st
  in foldl (\x y -> x + (weight (getObject y st))) 0 ls

-- assignment 5 3.1.1
-- checks whether the player is actually carrying an item
alreadyHaveTakeCheck :: ItemName -> GameState -> Error GameState
alreadyHaveTakeCheck thing st =
  let ls = inventory . player $ st
  in case (thing `elem` ls) of
    True -> Left ("You are already carrying the " ++ (show thing) ++ ".")
    False -> Right st

-- assignment 5 3.1.2
-- check whether item is in the correct room to pick up
inRoomTakeCheck :: ItemName -> GameState -> Error GameState
inRoomTakeCheck thing st =
  let things = nearbyObjects st
  in case (thing `elem` things) of
    True -> Right st
    False -> Left ("There is no " ++ (show thing) ++ " in this room.")

-- assignment 5 3.1.3
-- check whether the player can carry a new item based on max weight
weightCheck :: ItemName -> GameState -> Error GameState
weightCheck thing st =
  let total = inventoryWeight st
      maximum = maxWeight . player $ st
      heavy = weight (getObject thing st)
  in case ((total + heavy) > maximum) of
    True -> Left ("That's too much weight for you to carry.")
    False -> Right st

-- assignment 5 3.1.4
-- check whether user has item in inventory or in the room
anywhereDropCheck :: ItemName -> GameState -> Error GameState
anywhereDropCheck thing st =
  let inventory_things = inventory . player $ st
      room_things = nearbyObjects st
      x = thing `elem` inventory_things
      y = thing `elem` room_things
  in case (x || y) of
    True -> Right st
    False -> Left ("What do you mean, drop the " ++ (show thing) ++ "?")

-- assignment 5 3.1.5
-- check whether the thing is in the current room
inRoomDropCheck :: ItemName -> GameState -> Error GameState
inRoomDropCheck thing st =
  let things = nearbyObjects st
  in case (thing `elem` things) of
    True -> Left ("You aren't carrying the " ++ (show thing) ++ ".")
    False -> Right st

-- assignment 5 3.3.2
-- tells us in a current state whether the current room has stuff
roomHasObjects :: GameState -> Bool
roomHasObjects st = hasObjects $ currentRoom st

-- assignment 5 3.3.3
-- can we exit to a given direction and where would that take us
destinationName :: Direction -> Room -> Maybe RoomName
destinationName dir rm = lookup dir $ exits rm

-- move helper state
moveNewState :: RoomName -> GameState -> GameState
moveNewState place st = (GameState (message st)
                                   (gmap st)
                                   (universe st)
                                   (newLocation place (player st))
                                   (alreadyDone st)
                                   (hiddenTasks st))

-- assignment 5 3.3.4
-- move to a new location
move :: Direction -> GameState -> GameState
move dir st =
  let rm = currentRoom st
  in case (destinationName dir rm) of
    Nothing -> (setMessage "There is no exit in that direction." st)
    Just Tower ->
      case (GoToTower `elem` (todo . player $ st)) of
        True -> setMessage
                ("You go " ++ (show dir) ++ " to the tower."
                ++ "\nThere is a scroll on the parapet.")
                (moveNewState Tower st)
        False -> setMessage
                 ("You go " ++ (show dir) ++ " to the tower.")
                 (moveNewState Tower st)
    Just place ->
      setMessage ("You go " ++ (show dir) ++ " to the " ++ (show place) ++ ".")
                 (moveNewState place st)

-- add item to a specified room of the game
thingAppears :: ItemName -> RoomName -> GameState -> GameState
thingAppears thing rmname st =
  let rm = getRoom rmname st
      new_room = Room.addItem thing rm
      new_map = setRoomMap rmname new_room (gmap st)
  in (GameState (message st)
                (new_map)
                (universe st)
                (player st)
                (alreadyDone st)
                (hiddenTasks st))

-- remove item from inventory and the rest of the game
thingDisappears :: ItemName -> GameState -> GameState
thingDisappears thing st = (GameState (message st)
                                      (gmap st)
                                      (universe st)
                                      (Player.removeItem thing (player st))
                                      (alreadyDone st)
                                      (hiddenTasks st))

-- remove item from the room and the rest of the game
thingGone :: ItemName -> GameState -> GameState
thingGone thing st =
  let curr_room = currentRoom st
      newRoom = Room.removeItem thing curr_room
      newMap = setRoomMap (rname curr_room) newRoom (gmap st)
  in st{ gmap = newMap }

-- has the player picked up the scroll
takeScrollCheck :: GameState -> Bool
takeScrollCheck st = TakeScroll `elem` (alreadyDone st)

-- unlock task to be accomplished
unlockTasks :: TaskName -> GameState -> GameState
unlockTasks task st = GameState (message st)
                                (gmap st)
                                (universe st)
                                (addTask task (player st))
                                (alreadyDone st)
                                (delete task (hiddenTasks st))

completeTasksHelper :: TaskName -> GameState -> GameState
completeTasksHelper task st = GameState (message st)
                                        (gmap st)
                                        (universe st)
                                        (removeTask task (player st))
                                        ([task] ++ (alreadyDone st))
                                        (hiddenTasks st)

-- helper function to create new state when the scroll is picked up
newScrollState :: GameState -> GameState
newScrollState st =
  let step1 = thingDisappears Scroll st
      step2 = thingAppears Tome RoyalLibrary step1
      step3 = thingAppears Crown ThroneRoom step2
  in thingAppears Chest RoyalBedroom step3

-- cross an already completed task off to-do list
-- and perform other actions like scroll and key disappearing after one use
completeTasks :: TaskName -> GameState -> GameState
completeTasks task st =
  case task of
     TakeScroll -> completeTasksHelper task (newScrollState st)
     UnlockChest -> completeTasksHelper task
                       (thingAppears Paintbrush RoyalBedroom (thingGone Key st))
     TakeBook -> completeTasksHelper task (thingAppears Key RoyalLibrary st)
     _ -> completeTasksHelper task st

-- to-do list checker
-- iterate over current active to-do list
checkGoToTower :: GameState -> Bool
checkGoToTower st = (rname (currentRoom st) == Tower)

checkTakeBook :: GameState -> Bool
checkTakeBook st = ((Tome `elem` (currentInventory st))
                   && (TakeBook `elem` (todo . player $ st)))

checkTakeScroll :: GameState -> Bool
checkTakeScroll st = ((Scroll `elem` (currentInventory st))
                      && (TakeScroll `elem` (todo . player $ st)))

checkFindPaintbrush :: GameState -> Bool
checkFindPaintbrush st = (Paintbrush `elem` (currentInventory st))
                          && (FindPaintbrush `elem` (todo . player $ st))

checkFindCrown :: GameState -> Bool
checkFindCrown st = (Crown `elem` (currentInventory st))
                     && (FindCrown `elem` (todo . player $ st))

checkFindSceptre :: GameState -> Bool
checkFindSceptre st = (Sceptre `elem` (currentInventory st))
                       && (FindSceptre `elem` (todo . player $ st))

checkTakeKey :: GameState -> Bool
checkTakeKey st = (Key `elem` (currentInventory st))
                  && (TakeKey `elem` (todo . player $ st))

checkUnlockChest :: GameState -> Bool
checkUnlockChest st =
  let curr_loc = rname (currentRoom st)
      things = nearbyObjects st
  in ((curr_loc == RoyalBedroom) && (Chest `elem` things)
      && (Key `elem` things) && (UnlockChest `elem` (todo . player $ st)))

checkRevealDoor :: GameState -> Bool
checkRevealDoor st =
  let curr_loc = (currentRoom st)
  in (((rname curr_loc) == GreatHall)
     && ((W, RoyalGarden) `elem` (exits curr_loc))
     && (RevealDoor `elem` (todo . player $ st)))

-- assignment 5 3.4.1
-- define winning criteria
-- turning this into a to-do item
haveWonGame :: GameState -> Bool
haveWonGame st =
  let curr_loc = rname (currentRoom st)
      things = currentInventory st
  in ((curr_loc == Outside) && (Crown `elem` things) && (Sceptre `elem` things))

checkTodoListHelper :: TaskName -> GameState -> Bool
checkTodoListHelper task st =
  case task of
    GoToTower -> checkGoToTower st
    TakeScroll -> checkTakeScroll st
    TakeBook -> checkTakeBook st
    FindPaintbrush -> checkFindPaintbrush st
    FindCrown -> checkFindCrown st
    FindSceptre -> checkFindSceptre st
    TakeKey -> checkTakeKey st
    UnlockChest -> checkUnlockChest st
    RevealDoor -> checkRevealDoor st
    EscapeCastle -> haveWonGame st

checkTodoListHelperIterate :: [TaskName] -> GameState -> GameState
checkTodoListHelperIterate [] st = st
checkTodoListHelperIterate (task : xs) st =
  case (checkTodoListHelper task st) of
    False -> checkTodoListHelperIterate xs st
    True -> checkTodoListHelperIterate xs (completeTasks task st)

-- crosses out completed tasks and moves to already done
checkTodoList :: GameState -> GameState
checkTodoList st =
  let activeList = todo . player $ st
  in checkTodoListHelperIterate activeList st

-- unlock certain tasks and move to active to-do list
addTakeScroll :: GameState -> Bool
addTakeScroll st = (rname (currentRoom st) == Tower)

addTakeBook :: GameState -> Bool
addTakeBook st = takeScrollCheck st

addFindPaintbrush :: GameState -> Bool
addFindPaintbrush st = takeScrollCheck st

addFindCrown :: GameState -> Bool
addFindCrown st = takeScrollCheck st

addFindSceptre :: GameState -> Bool
addFindSceptre st = takeScrollCheck st

addTakeKey :: GameState -> Bool
addTakeKey st = (Tome `elem` currentInventory st)

addUnlockChest :: GameState -> Bool
addUnlockChest st = (Key `elem` currentInventory st)

addRevealDoor :: GameState -> Bool
addRevealDoor st = (Paintbrush `elem` currentInventory st)

addEscapeCastle :: GameState -> Bool
addEscapeCastle st = takeScrollCheck st

addTodoListHelper :: TaskName -> GameState -> Bool
addTodoListHelper task st =
  case task of
    TakeScroll -> addTakeScroll st
    TakeKey -> addTakeKey st
    UnlockChest -> addUnlockChest st
    RevealDoor -> addRevealDoor st
    _ -> takeScrollCheck st

addTodoListHelperIterate :: [TaskName] -> GameState -> GameState
addTodoListHelperIterate [] st = st
addTodoListHelperIterate (task : xs) st =
  case (addTodoListHelper task st) of
    False -> addTodoListHelperIterate xs st
    True -> addTodoListHelperIterate xs (unlockTasks task st)

-- adds hidden tasks to active list
addTodoList :: GameState -> GameState
addTodoList st =
  let hiddenList = hiddenTasks st
  in addTodoListHelperIterate hiddenList st

-- combine checking for list completion and spawning new items
todoMainFunc :: GameState -> GameState
todoMainFunc st = addTodoList (checkTodoList st)