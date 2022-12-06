module GameIO where

import Control.Monad.State
import System.Exit
import System.IO

import GameState
import Player
import Room
import Command
import Item

-- assignment 8 1.5.1
-- establish datatype
type GameIO a = StateT GameState IO a

-- assignment 8 1.5.2
-- create a visible input prompt for the user
prompt :: GameIO ()
prompt = lift (putStr "-> " >> hFlush stdout)

-- assignment 8 1.5.3
-- prints the message of the current state of the game
printMessage :: GameIO ()
printMessage = do
  state <- get
  case message state of
    Just m -> lift $ putStrLn m
    Nothing -> pure ()
  put $ setMessage "" state

-- assignment 8 1.5.4
-- prints the description of the room where the player is
printDescription :: GameIO ()
printDescription = do
  state <- get
  let description = desc . currentRoom $ state
  lift $ putStrLn description

-- assignment 8 1.5.5
-- print the objects present in the room where the player is
printObjects :: GameIO ()
printObjects = do
  state <- get
  let objs = nearbyObjects state
  case objs of
    [] -> pure ()
    (x : xs) -> do
--      lift $ putStrLn "You see the following objects:"
--      fmap putStrLn objs
      lift . putStrLn . foldl (\acc item -> acc <> "\n" <> show item)
                              "You see the following objects:"
                              $ objs
--        traverse_ (lift . putStrLn . show) objs

-- assignment 8 2.0.1
-- prints the exits
printExits :: GameIO ()
printExits = do
  state <- get
  let list_exits = exits . currentRoom $ state
  case list_exits of
    [] -> pure ()
    (x : xs) -> do
      lift . putStrLn . foldl (\acc exit -> acc <> "\n" <> show (fst exit))
                              "There are exits in the following directions:"
                              $ list_exits

-- assignment 8 2.0.2
-- prints the inventory carried by the player
printInventory :: GameIO ()
printInventory = do
  state <- get
  let stuff = currentInventory state
  case stuff of
    [] -> lift $ putStrLn "You aren't carrying anything."
    (x : xs) -> do
      lift . putStrLn . foldl (\acc item -> acc <> "\n" <> show item)
                              "You are carrying the following objects:"
                              $ stuff

scrollMessage :: String
scrollMessage = "You probably have noticed that everyone has disappeared.\n" ++
                "We can't explain too much here, but if you want to find " ++
                "out what happened, you will need to make it outside.\n" ++
                "We know the door has vanished, so you will need the magic " ++
                "paintbrush that we gave you on your sixteenth birthday.\n" ++
                "You can only use it once, but this is definitely the time " ++
                "to use it. \nPaint the wall in the direction the door used " ++
                "to be and it will reveal itself. \nTo provide you with some" ++
                " help, you will need to first find the book of knowledge.\n" ++
                "When you leave the grounds, be sure to also take the " ++
                "kingdom crown and sceptre with you as a mark of royalty.\n" ++
                "We're counting on you.\nMom and Dad"

-- message in the scroll
printScroll :: GameIO ()
printScroll = do
  state <- get
  case (takeScrollCheck state) of
    False -> lift $ putStrLn "I don't understand that."
    True -> lift $ putStrLn scrollMessage

-- prints the instructions list
printInstructions :: GameIO ()
printInstructions = do
  state <- get
  lift $ putStrLn "Here are the instructions to this game:"
  lift $ putStrLn "Type 'todo' to take a look at your to-do list."
  lift $ putStrLn "Type 'story' to read the story of this game again."
  lift $ putStrLn "Type 'look' to have a look around the room."
  lift $ putStrLn ("Type 'take <item name>' to take something in the room. "
                  ++
                  "For instance, type 'take goblet' to take the goblet.")
  lift $ putStrLn ("Type 'drop <item name>' to put something down in the room. "
                  ++
                  "For instance, type 'drop goblet' to put the goblet down.")
  lift $ putStrLn ("Type '<direction>' to move through room exits. " ++
                  "For instance, type 'east' to go through the eastern exit.")
  lift $ putStrLn ("Type 'inventory' to check your current inventory.")
  case (takeScrollCheck state) of
    True -> lift $ putStrLn "Type 'scroll' to read the scroll again."
    False -> pure ()
  case (foundPaintbrush state) of
    True -> lift $ putStrLn ("Type 'paint <direction>' to paint the wall of " ++
                             "the direction of the area. For instance, type " ++
                             "'paint east' to paint the eastern wall.")
    False -> pure ()
  lift $ putStrLn "Type 'map' to show the areas of the castle."
  lift $ putStrLn "Type 'exit' or 'quit' to quit the game."
  lift $ putStrLn "Type 'help' to pull up this instructions list again."

printStory :: GameIO ()
printStory = do
  lift $ putStr "You are the heir to the throne of your kingdom, "
  lift $ putStrLn "but you woke up to discover that everyone has disappeared! "
  lift $ putStr "The door leading outside to the royal garden from the west "
  lift $ putStrLn "of the Great Hall has vanished, leaving you trapped inside. "
  lift $ putStr "You must figure out a way to exit the castle "
  lift $ putStrLn "to get answers about what happened."

printTodoList :: GameIO ()
printTodoList = do
  state <- get
  let tasks = currentTodoList state
  case tasks of
    [] -> lift $ putStrLn "Your to-do list is empty."
    (x : xs) -> do
      lift . putStrLn . foldl (\acc task -> acc <> "\n" <> show task)
                              "Your to-do list:"
                              $ tasks

showMap :: String
showMap =
  ("    Tower\n" ++
   "      |\n" ++
   "Royal library\n" ++
   "      |\n" ++
   "  Great Hall -- Throne room\n" ++
   "      |\n" ++
   "Royal bedroom")

showMapUnlocked :: String
showMapUnlocked =
  ("                              Tower\n" ++
   "                                |\n" ++
   "                         Royal library\n" ++
   "                                |\n" ++
   "Outside -- Royal garden -- Great Hall -- Throne room\n" ++
   "                                |\n" ++
   "                         Royal bedroom")

printMap :: GameIO ()
printMap = do
  state <- get
  case (revealDoorCheck state) of
    True -> lift $ putStrLn showMapUnlocked
    False -> lift $ putStrLn showMap

-- assignment 8 2.0.3
-- implements the function as inputted
actionOverList :: (ItemName -> GameState -> GameState) ->
                  [ItemName] -> GameIO ()
actionOverList func things = do
  state <- get
  case things of
    [] -> pure ()
    (x : xs) -> do
      put $ func x state
      printMessage
      actionOverList func xs

-- assignment 8 2.0.4
-- print success message
finishGame :: GameIO ()
finishGame = do
  lift $ putStrLn
    ("You made it outside! Time to figure out what happened to everyone." ++
    "\nCongratulations! You have won the game!" ++
    "\nType 'exit' or 'quit' to quit the game.")
--  lift exitSuccess

-- assignment 8 2.0.5
-- quits the game when the user wants to go instead of winning
exit :: GameIO ()
exit = do
  lift $ putStrLn "Goodbye!"
  lift exitSuccess

-- assignment 8 2.0.6
-- has the player won yet?
checkGameOver :: GameIO ()
checkGameOver = do
  state <- get
  let status = haveWonGame state
  case status of
    False -> pure ()
    True -> finishGame

-- check the state of all active to-do list items
todoChecker :: GameIO ()
todoChecker = do
  state <- get
  put $ todoMainFunc state

-- assignment 8 2.0.7
-- error message for wrong syntaxes
syntaxError :: GameIO ()
syntaxError = do
  lift $ putStrLn "I don't understand that."

-- assignment 8 2.0.8
-- welcome to the game!
opening :: GameIO ()
opening = do
  lift $ putStrLn "\nWelcome to Functional Adventure: the Silent Castle!"
  lift $ putStrLn
          "Type 'help' to get a list of available moves in the game.\n"
  printStory
  lift $ putStrLn "\nFeeling dazed, you decide to go up to the tower."
  lift $ putStrLn "Maybe seeing your kingdom through the telescope will help."

-- assignment 8 2.0.9
-- takes commands and executes
performCommand :: Command -> GameIO ()
performCommand cmd = do
  state <- get
  case cmd of
    Look -> printDescription >> printObjects >> printExits
    Move dir -> do
      put $ move dir state
      printMessage
    Inventory -> printInventory
    Take items -> actionOverList takeItem items
    Drop items -> actionOverList dropItem items
    Instructions -> printInstructions
    Story -> printStory
    Todo -> printTodoList
    Paint dir -> do
      put $ paintDoor dir state
      printMessage
    ReadScroll -> printScroll
    ShowMap -> printMap
    Exit -> exit

-- assignment 8 2.0.10
-- perform commands in succession
performConjunction :: Conjunction -> GameIO ()
performConjunction cmds = do
  state <- get
  case cmds of
    [] -> pure ()
    (x : xs) -> performCommand x >> todoChecker >> performConjunction xs

-- assignment 8 2.0.11
-- parses the string and runs the commands listed
parseConjunction :: String -> GameIO ()
parseConjunction str = do
  state <- get
  let cmds = parseInput str
  case cmds of
    Nothing -> syntaxError
    Just xs -> performConjunction xs

-- assignment 8 2.0.12
-- imitates repl behavior on the game state
repl :: GameIO ()
repl = do
  state <- get
  prompt
  str <- lift getLine
  parseConjunction str
  checkGameOver