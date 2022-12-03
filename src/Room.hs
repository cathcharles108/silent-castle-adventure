module Room where

import Data.List
import Item
import Direction
import Tasks

--type RoomName = String
type Exit = (Direction, RoomName)

data RoomName
  = GreatHall
  | ThroneRoom
  | RoyalBedroom
  | RoyalLibrary
  | Tower
  | RoyalGarden
  | Outside
  deriving (Eq, Ord)

instance Show RoomName where
  show GreatHall = "Great Hall"
  show ThroneRoom = "throne room"
  show RoyalBedroom = "royal bedroom"
  show RoyalLibrary = "royal library"
  show Tower = "tower"
  show RoyalGarden = "royal garden"
  show Outside = "outside world"

data Room
  = Room { rname :: RoomName
         , desc :: String
         , exits :: [Exit]
         , objects :: [ItemName] }
  deriving (Show, Eq)

greatHall :: Room
greatHall =  Room GreatHall
             ("You are in the Great Hall, a beautiful and majestic space " ++
             "with tapestries of the kingdom and stained-glass windows.")
             [(N, RoyalLibrary),(S, RoyalBedroom),(E, ThroneRoom)]
             [Tapestry, Goblet, Candelabra]

royalGarden :: Room
royalGarden = Room RoyalGarden
              ("You are in the royal garden, full of your favorite plants " ++
              "and adorned with topiaries and a centerpiece fountain. " ++
              "To the west is a beautiful arch with a gate that leads outside.")
              [(W, Outside), (E, GreatHall)]
              [Sceptre, Topiary, Vase]

outside :: Room
outside = Room Outside
          ("You made it outside! But remember to bring the kingdom crown " ++
          "and sceptre with you before you start searching for answers.")
          [(E, RoyalGarden)]
          []

throneRoom :: Room
throneRoom = Room ThroneRoom
             ("You are in the throne room, where the King and Queen have " ++
             "their thrones. Bask in the might of our beloved monarchy.")
             [(W, GreatHall)]
             [Throne, Painting]

royalBedroom :: Room
royalBedroom = Room RoyalBedroom
               ("You are in the royal bedroom, a place for you to rest in " ++
               "after a long day and filled with all your favorite belongings.")
               [(N, GreatHall)]
               [Bed, Mirror]

royalLibrary :: Room
royalLibrary = Room RoyalLibrary
               ("You are in the royal library, your favorite place to " ++
               "unwind and read a good tome. Legend has it that it is "  ++
               "filled with secrets even you do not know.")
               [(N, Tower),(S, GreatHall)]
               [Parchment, Quill]

tower :: Room
tower = Room Tower
        ("You are in the tower, where there is your favorite telescope " ++
        "to see the stars and the beautiful kingdom.")
        [(S, RoyalLibrary)]
        [Telescope, Scroll]

roomNames :: [RoomName]
roomNames = map rname allRooms

-- assignment 4 1.5.2
-- add item to the room
addItem :: ItemName -> Room -> Room
addItem thing room = Room (rname room)
                           (desc room)
                           (exits room)
                           (thing : objects room)

-- assignment 4 1.5.3
removeItem :: ItemName -> Room -> Room
removeItem thing room = Room (rname room)
                              (desc room)
                              (exits room)
                              (delete thing (objects room))

-- assignment 4 3.2.1
-- list of all rooms defined
allRooms :: [Room]
allRooms = [greatHall,throneRoom,royalBedroom,royalLibrary,tower,
            royalGarden, outside]

-- assignment 5 3.3.1
-- does a room have objects or no?
hasObjects :: Room -> Bool
hasObjects rm = not ((objects rm) == [])