module Item where

import qualified Data.Map as M

--type ItemName = String
type Universe = M.Map ItemName Item

data Item
  = Item { iname :: ItemName
         , weight :: Integer }
  deriving (Show, Eq)

data ItemName
  = Tapestry
  | Goblet
  | Candelabra
  | Tome
  | Parchment
  | Quill
  | Telescope
  | Bed
  | Mirror
  | Chest
  | Throne
  | Crown
  | Painting
  | Paintbrush
  | Sceptre
  | Topiary
  | Vase
  | Scroll
  | Key
  deriving (Eq, Ord)

instance Show ItemName where
  show Tapestry = "tapestry"
  show Goblet = "goblet"
  show Candelabra = "candelabra"
  show Tome = "tome"
  show Parchment = "parchment"
  show Quill = "quill"
  show Telescope = "telescope"
  show Bed = "bed"
  show Mirror = "mirror"
  show Chest = "chest"
  show Throne = "throne"
  show Crown = "crown"
  show Painting = "painting"
  show Paintbrush = "magic paintbrush"
  show Sceptre = "sceptre"
  show Topiary = "topiary"
  show Vase = "vase"
  show Scroll = "scroll"
  show Key = "key"

-- creating a Universe out of the list of items in game
mkUniverse :: [Item] -> Universe
mkUniverse items =
  let mkUniverse' = map (\x -> (iname x, x))
  in M.fromList (mkUniverse' items)

tapestry :: Item
tapestry = Item Tapestry 20

goblet :: Item
goblet = Item Goblet 2

candelabra :: Item
candelabra = Item Candelabra 5

tome :: Item
tome = Item Tome 25

parchment :: Item
parchment = Item Parchment 1

quill :: Item
quill = Item Quill 1

telescope :: Item
telescope = Item Telescope 30

bed :: Item
bed = Item Bed 100

mirror :: Item
mirror = Item Mirror 35

chest :: Item
chest = Item Chest 50

throne :: Item
throne = Item Throne 75

crown :: Item
crown = Item Crown 5

painting :: Item
painting = Item Painting 7

paintbrush :: Item
paintbrush = Item Paintbrush 2

sceptre :: Item
sceptre = Item Sceptre 10

topiary :: Item
topiary = Item Topiary 35

vase :: Item
vase = Item Vase 18

scroll :: Item
scroll = Item Scroll 1

key :: Item
key = Item Key 1

univ :: Universe
univ = mkUniverse [tapestry, goblet, candelabra, tome, parchment, quill,
                   telescope, bed, mirror, chest, throne, crown, painting,
                   paintbrush, sceptre, topiary, vase, scroll, key]

itemNames :: [ItemName]
itemNames = M.keys univ

-- helper value for Example Player instance
itemList :: [Item]
itemList = M.elems univ