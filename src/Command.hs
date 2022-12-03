module Command where

import Text.Parsec hiding
  ( parse
  , choice
  , (<|>)
  , sepBy
  , sepBy1
  , many
  , many1
  )
import qualified Text.Parsec as P
import Text.Parsec.String (Parser)

import Item
import Direction

data Command
  = Inventory
  | Look
  | Drop [ItemName]
  | Take [ItemName]
  | Move Direction
  | Instructions
  | Story
  | Exit
  | Todo
  | Paint Direction
  | ReadScroll
  deriving (Eq, Show)

type Conjunction = [Command]

(<|>) :: Parser a -> Parser a -> Parser a
prsr1 <|> prsr2 = (P.<|>) (try prsr1) prsr2

choice :: [Parser a] -> Parser a
choice = P.choice . map try

sepBy :: Parser a -> Parser b -> Parser [a]
sepBy body sep = P.sepBy1 body (P.try sep)

sepBy1 :: Parser a -> Parser b -> Parser [a]
sepBy1 body sep = P.sepBy1 body (P.try sep)

many :: Parser a -> Parser [a]
many = P.many . try

many1 :: Parser a -> Parser [a]
many1 = P.many1 . try

parse :: Parser a -> String -> Either ParseError a
parse prsr = P.parse prsr ""

-- assignment 7 1.4
itemNameP :: Parser ItemName
itemNameP =
  let eachItem = (\itmnm -> pure itmnm <* string (show itmnm))
  in choice $ map eachItem itemNames

-- assignment 7 1.5.1
nounPhrase_stub :: Parser [ItemName]
nounPhrase_stub = do
  thing <- itemNameP
  pure [thing]

-- assignment 7 2.0.8
nounPhrase :: Parser [ItemName]
nounPhrase = (sepBy (itemNameP) ((string ", ") <|> (string ",")))

-- assignment 7 1.5.2
inventoryP :: Parser Command
inventoryP = pure Inventory <* string "inventory"

-- assignment 7 1.5.3
takeP :: Parser Command
takeP = do
  thing <- (string "take" *> char ' ' *> nounPhrase)
  pure (Take thing)

-- assignment 7 1.5.4
exitP :: Parser Command
exitP = pure Exit <* (string "exit" <|> string "quit")

-- assignment 7 2.0.1
dropP :: Parser Command
dropP = do
  thing <- (string "drop" *> char ' ' *> nounPhrase)
  pure (Drop thing)

-- assignment 7 2.0.2
lookP :: Parser Command
lookP = pure Look <* string "look"

-- assignment 7 2.0.3
directionP :: Parser Direction
directionP =
  let eachDir = (\dir -> pure dir <* string (show dir))
  in choice $ map eachDir [N, S, E, W]

-- assignment 7 2.0.4
moveP :: Parser Command
moveP = do
  dir <- directionP
  pure (Move dir)

instructionsP :: Parser Command
instructionsP = pure Instructions <* string "instructions"

storyP :: Parser Command
storyP = pure Story <* string "story"

todoP :: Parser Command
todoP = pure Todo <* string "todo"

paintP :: Parser Command
paintP = do
  dir <- (string "paint" *> char ' ' *> directionP)
  pure (Paint dir)

readScrollP :: Parser Command
readScrollP = pure ReadScroll <* string "scroll"

-- assignment 7 2.0.5
commandP :: Parser Command
commandP = choice [inventoryP, lookP, takeP, dropP, paintP, readScrollP,
                  moveP, instructionsP, storyP, todoP, exitP]

-- assignment 7 2.0.6
conjunctionP :: Parser Conjunction
conjunctionP = (sepBy (commandP) (string " and ")) <* eof

-- assignment 7 2.0.7
parseInput :: String -> Maybe Conjunction
parseInput s =
  case (parse conjunctionP s) of
    Left _ -> Nothing
    Right xs -> Just xs