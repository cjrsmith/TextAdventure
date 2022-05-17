import Base
import System.IO


--Q1
opposite :: Direction -> Direction
opposite North = South
opposite East = West
opposite South = North
opposite West = East

--Q2
noActions :: Item -> GameState -> Next GameState
noActions item state = Same $ "Nothing can be done with the " ++ show item  ++ " here."

--Q5
action :: Item -> GameState -> Next GameState
action Spoon (GS p r) =
    if null (monsters r)
    then
        Same "Nothing can be done here"
    else
        let x = head (monsters r) in
            case x of
                (WoodTroll h hl a)-> if h <= 5
                                        then Progress "You killed the troll" (GS p r{monsters = [], items = [(Key, "On the floor")]})
                                        else Progress "You've hurt the troll" (GS p r{monsters = [WoodTroll 5 Key (Club 5)]})
                (Owlbear h a)     -> if h <= 5
                                        then Progress "You killed the owlbear" (GS p r{monsters = []})
                                        else Progress "You hurt the owlbear" (damages 5 (Right x) (GS p r))
action Sword (GS p r) =
    if null (monsters r)
    then
        Same "Nothing can be done here"
    else
        let x = head (monsters r) in
            case x of
                (WoodTroll h hl a)-> Progress "You killed the troll" (GS p r{monsters = [], items = [(Key, "On the floor")]})
                (Owlbear h a)     -> if h <= 10
                                        then Progress "You killed the owlbear" (GS p r{monsters = []})
                                        else Progress "You hurt the owlbear" (damages 10 (Right x) (GS p r))
action _ (GS p r) = Same "Nothing can be done here"

--Q5 (bonus)
damages :: Int -> Either Player Monster -> GameState -> GameState
damages a (Left pl) (GS p r) = GS p{hlth = hlth pl - a} r
damages a (Right m) (GS p r) = GS p r{monsters = [m{health = health m - a}]}

--Q3
--Winning room
billiardsRoom :: Room
billiardsRoom = Room "Billiards Room" "bright and well preserved" True (Just Key) [] [] [(West, courtyard)] noActions

--Q4
--Starting room
courtyard :: Room
courtyard = Room "Courtyard" "overgrown with ivy and surrounded by eerie statues" False Nothing [(Spoon, "On the floor")] [] [(West, foyer), (East, billiardsRoom), (North, garage), (South, forest)] noActions

--Q5
--Foyer with a troll in
foyer :: Room
foyer = Room "Foyer" "dark and damp" False Nothing [] [WoodTroll 10 Key (Club 5)] [(East, courtyard)] action

--Garage with a sword
garage :: Room
garage = Room "Garage" "dusty, you feel like you're being watched" False Nothing [(Sheild , "On the wall")] [] [(South, courtyard)] noActions

--Forest with an owlbear and a sword
forest :: Room
forest = Room "Forgotten Forest" "dark and quiet, maybe too quiet. 'Hmm,' you think, 'whats that over there?'" False Nothing [(Sword, "In a stone")] [Owlbear 20 (Bite 10)] [(North, courtyard)] action

--Q6
player0 :: Player
player0 = Player "Adventurer" [] 10
--Q6
game0 :: GameState
game0 = GS player0 courtyard

--Q7
instance Parsable Item where
    parse "key" = Just Key
    parse "spoon" = Just Spoon
    parse "sword" = Just Sword
    parse "sheild" = Just Sheild
    parse _ = Nothing
--Q8
instance Parsable Direction where
    parse "north" = Just North
    parse "east" = Just East
    parse "south" = Just South
    parse "west" = Just West
    parse _ = Nothing
--Q9
instance Parsable Command where
    parse ('g':'o':' ':xs) =
        case parse xs of
            Nothing -> Nothing
            Just x -> Just (Move x)
    parse ('g':'r':'a':'b':' ':xs) =
        case parse xs of
            Nothing -> Nothing
            Just x -> Just (PickUp x)
    parse ('u':'s':'e':' ':xs) =
        case parse xs of
            Nothing -> Nothing
            Just x -> Just (Use x)
    parse "end" = Just End
    parse "help" = Just Help
    parse _ = Nothing

--Q10
tellResponse :: String -> IO ()
tellResponse a = putStrLn $ "< " ++ a ++ "."

--Q11
readCommand :: IO (Maybe Command)
readCommand = putStr "> " >> hFlush stdout >> (getLine >>= (\inp -> return (parse inp)))

--Q12
deleteFrom :: Eq a => a -> [(a,b)] -> [(a,b)]
deleteFrom item [] = []
deleteFrom item ((k, v): xs) | item == k = deleteFrom item xs
                             | otherwise = (k,v) : deleteFrom item xs

--Q13
leaveRoom:: Room -> Direction -> Room -> Room
leaveRoom src dir dst = dst {doors = (opposite dir, src):deleteFrom (opposite dir) (doors dst)}

--Q14
step :: Command -> GameState -> Next GameState
-----a)
step (Move dir) (GS p r) =
    case lookup dir (doors r) of
    Just n ->
        case requires n of
        Nothing   -> Progress ("You go through the door to the " ++ show dir) (GS p (leaveRoom r dir n))
        Just item -> if elem item (inventory p)
            then Progress ("You unlock the door to the " ++ show dir) (GS p (leaveRoom r dir n))
            else Same "You don't have the key to unlock this door"
    Nothing -> Same "There is no door in that direction"
-----b)
step (PickUp item) (GS p r) =
    case lookup item (items r) of
    Nothing -> Same "It appears this item is not in this room"
    Just a  -> Progress ("You picked up the " ++ show item) (GS p{inventory = item : inventory p} r{items = deleteFrom item (items r)})
-----c)
step (Use item) (GS p r) =
    if elem item (inventory p)
        then (actions r) item (GS p r)
        else Same $ "You do not have a " ++ show item

--Q15
play :: GameState -> IO ()
play gs = tellContext gs >> playLoop gs
--Q15
playLoop :: GameState  -> IO ()
playLoop (GS p r)
  | hlth p <= 0 = tellResponse "You died!" >> return ()
  | isWinRoom r = tellResponse ("You won " ++ playerName p ++ ". Congratulations!") >> return ()
  | otherwise =
    do
    cmd <- readCommand
    case cmd of
        Nothing   -> tellResponse "Unknown command" >> playLoop (GS p r)
        Just End  -> do {tellResponse "Goodbye"; return()}
        Just Help -> do
                     putStrLn ""
                     putStrLn "----------------------Help menu--------------------------"
                     putStrLn ""
                     putStrLn "go <Direction>, (Move in a direction e.g 'go north')"
                     putStrLn "use <Item>, (Use an item in your inventory e.g 'use spoon')"
                     putStrLn "grab <Item>, (Pickup an item in a room e.g 'grab key')"
                     putStrLn "end, (Ends the game at any point)"
                     putStrLn ""
                     playLoop (GS p r)
        Just a   -> case step a (GS p r) of
                        Same s                    -> do {tellResponse s; playLoop (GS p r)}
                        Progress s (GS new state) -> do
                                            tellResponse s
                                            tellContext (GS new state)
                                            if null (monsters state)
                                                then playLoop (GS new state)
                                                else
                                                    if elem Sheild (inventory new) && not (null (monsters state))
                                                        then tellResponse "Your sheild protects you from the monsters attack" >> playLoop (GS new state)
                                                        else let x                = head (monsters state)
                                                                 (GS player room) = (damages (damage (attack x)) (Left p) (GS new state))
                                                            in tellResponse ("The monster " ++ show (attack x) ++ ", your health is " ++ show player) >> playLoop (GS player room)

--Q16
main :: IO ()
main = play game0
