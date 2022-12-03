module Tasks where

data TaskName
  = GoToTower
  | TakeScroll
  | TakeBook
  | FindPaintbrush
  | FindCrown
  | FindSceptre
  | TakeKey
  | UnlockChest
  | RevealDoor
  | EscapeCastle
  deriving Eq

instance Show TaskName where
  show GoToTower = "- Head up to the tower"
  show TakeScroll = "- Take the odd-looking scroll on the parapet of the tower"
  show TakeBook = "- Find the book of knowledge"
  show FindPaintbrush = "- Find your magic paintbrush"
  show FindCrown = "- Find the crown of the kingdom"
  show FindSceptre = "- Find the sceptre of the kingdom"
  show TakeKey = "- Take the key"
  show UnlockChest = "- Bring key to room with the chest and put the key down"
  show RevealDoor =
    "- Use the paintbrush on the west wall of the Great Hall to reveal the door"
  show EscapeCastle =
    "- Bring the crown and sceptre with you and leave the castle"

-- list of all tasks that are initially hidden from the player
allTasks :: [TaskName]
allTasks = [TakeScroll
           , TakeBook
           , FindPaintbrush
           , FindCrown
           , FindSceptre
           , TakeKey
           , UnlockChest
           , RevealDoor
           , EscapeCastle]