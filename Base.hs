module Base where

import Data.List

-- # Core types

-- ## Game State
-- The game state has a player and a room (the current room)
data GameState = GS { player :: Player, room :: Room }

-- The `Next` type is used to represent what happens next in a game
-- either things stay the `Same` or `Progress` is made to a new game state
data Next a =
      Same String       -- Message about why things are staying the same
    | Progress String a -- Success message and next thing `a`

-- ## Player
-- Has a name a list of items
data Player =
  Player
    {   playerName :: String
      , inventory  :: [Item]
      , hlth :: Int
    }
instance Show Player where
  show (Player n i h) = show h

-- ## Room (where all the action happens)

data Room =
  Room
    {  name        :: String
     , description :: String
     , isWinRoom   :: Bool
     , requires    :: Maybe Item
       -- ^ whether an item is required to enter this room or not
     , items       :: [(Item, String)]
       -- ^ Association list of items and some description about where they are
     , monsters    :: [Monster]
       -- ^ Some monsters in this room
     , doors       :: [(Direction, Room)]
       -- ^ Association list between directions and a room
     , actions     :: Item -> GameState -> Next GameState
       -- ^ Function taking an item and a game state and returning what to do next
    }

data Direction = North | South | East | West
  deriving Eq

instance Show Direction where
  show North  = "north"
  show South  = "south"
  show East   = "east"
  show West   = "west"

-- ## Items and monsters
data Item = Key | Spoon | Sword | Sheild
  deriving Eq

instance Show Item where
  show Key   = "key"
  show Spoon = "spoon"
  show Sword = "sword"
  show Sheild = "sheild"

data Monster = WoodTroll { health :: Int, holding :: Item, attack :: Attack } | Owlbear {health :: Int, attack :: Attack}

instance Show Monster where
  show (WoodTroll health item attack) = "wood troll holding a " ++ show item
  show (Owlbear health attack) = "ferocious owlbear. Watch out for her beak"

-- Monster attacks
data Attack = Club {damage :: Int} | Bite {damage :: Int}
  deriving Eq 

instance Show Attack where
  show (Club d) = "clubs you and deals -" ++ show d ++ " damage"
  show (Bite d)  = "bites you and deals -" ++ show d ++ " damage"

-- ## Command interface

data Command =
  Move Direction | Use Item | PickUp Item | End | Help

-- ## Key type class

class Parsable t where
  parse :: String -> Maybe t

-- ## Helpers for outputing to the user

tellContextLine :: String -> IO ()
tellContextLine s = putStrLn $ "   " ++ s ++ "."

tellDoors :: [(Direction, Room)] -> IO ()
tellDoors [] = tellContextLine $ "There are no doors."
tellDoors [(dir, _)] = tellContextLine $ "There is a door to the " ++ show dir
tellDoors doors =
  tellContextLine $ "There are doors to the " ++ (intercalate " and " (map (show . fst) doors))

tellItem :: (Item, String) -> IO ()
tellItem (item, pos) = tellContextLine $ pos ++ " there is a " ++ show item

tellMonster :: Monster -> IO ()
tellMonster monster = tellContextLine $ "There is a " ++ show monster

-- Main help for taking a game state and outputting information about it
tellContext :: GameState -> IO ()
tellContext (GS p r) = do
  putStrLn ""
  tellContextLine $ "You are in a " ++ name r ++ ". It is " ++ description r
  tellDoors (doors r)
  mapM tellItem (items r)
  mapM tellMonster (monsters r)
  putStrLn ""
  return ()