module Engine where

import Prelude hiding (lookup)
import World
import Data.Map (Map, fromList, (!), lookup)

adjacent_cell :: Pos -> Dir -> Pos
adjacent_cell (x, y) East      = (x+1, y)
adjacent_cell (x, y) SouthEast = if even y then (x, y+1) else (x+1, y+1)
adjacent_cell (x, y) SouthWest = if even y then (x-1, y+1) else (x, y+1)
adjacent_cell (x, y) West      = (x-1, y)
adjacent_cell (x, y) NorthWest = if even y then (x-1, y-1) else (x, y-1)
adjacent_cell (x, y) NorthEast = if even y then (x, y-1) else (x+1, y-1)

-- Define turn function which takes a direction to turn and
-- orientation direction and returns a new orientation
-- (section 2.1, page 4)

turn :: Left_Or_Right -> Dir -> Dir
turn TurnLeft  East      = NorthEast
turn TurnLeft  SouthEast = East
turn TurnLeft  SouthWest = SouthEast
turn TurnLeft  West      = SouthWest
turn TurnLeft  NorthWest = West
turn TurnLeft  NorthEast = NorthWest
turn TurnRight East      = SouthEast
turn TurnRight SouthEast = SouthWest
turn TurnRight SouthWest = West
turn TurnRight West      = NorthWest
turn TurnRight NorthWest = NorthEast
turn TurnRight NorthEast = East

-- Define sensed_cell function that takes a position, direction
-- and a direction to sense in and returns the position that will
-- be sensed (section 2.1, page 4) 

sensed_cell :: Pos -> Dir -> Sense_Dir -> Pos
sensed_cell p d Here       = p
sensed_cell p d Ahead      = adjacent_cell p d
sensed_cell p d LeftAhead  = adjacent_cell p (turn TurnLeft d)
sensed_cell p d RightAhead = adjacent_cell p (turn TurnRight d)

-- Define kill_ant_at function (section 2.3, page 5)

kill_ant_at :: Pos -> World -> World
kill_ant_at p w = clear_ant_at p w

-- Define anthill_at function (section 2.3, page 6)

anthill_at :: Pos -> Color -> World -> Bool
anthill_at p c w = case lookup p (hills w) of
                    Just hillColor -> (c == hillColor)
                    Nothing -> False
                    

-- Define cell_matches function (section 2.6, page 7 and 8)

cell_matches :: Pos -> Condition -> Color -> World -> Bool
cell_matches p cond c w = 
    if rocky p w then
        if cond == Rock then True else False
    else case cond of
        Friend         -> some_ant_is_at p w && color (ant_at p w) == c
        Foe            -> some_ant_is_at p w && color (ant_at p w) /= c
        FriendWithFood -> some_ant_is_at p w && color (ant_at p w) == c
                                                 && has_food (ant_at p w)
        FoeWithFood    -> some_ant_is_at p w && color (ant_at p w) /= c
                                                 && has_food (ant_at p w)
        Food -> food_at p w > 0
        Rock -> False -- rocky checked before
        Marker i ->  check_marker_at p c i w
        FoeMarker -> check_any_marker_at p (other_color c) w
        Home ->    anthill_at p c w
        FoeHome -> anthill_at p (other_color c) w


-- Define a Game type

data Game = Game { gameRound :: Integer
                 , random :: [Integer]
                 , instructions :: Map Color (Map InsState Instruction)
                 , world :: World
                 }
                
mkGame :: Integer -> [(Color, [Instruction])] -> World -> Game
mkGame seed bs w = Game { gameRound = 0
                        , random = randomIntStream seed
                        , instructions = fromList (map f bs)
                        , world = w
                        }
    where
        f :: (Color, [Instruction]) -> (Color, Map InsState Instruction)
        f (c, ins) = (c, fromList (zip [0..] ins))
        
                 
-- Define get_instruction function (page 8)

get_instruction :: Color -> InsState -> Game -> Instruction
get_instruction c s g = ins ! c ! s
    where ins = instructions g

-- Define helper functions that will allow easy translation of the 
-- pseudo code for the step function.

-- aa = adjusts an ant and sets a value
-- aw = adjusts a world
-- qw = query the world

aa :: Pos -> (Ant -> v -> Ant) -> v -> Game -> Game
aa p f v g = g { world = set_ant_at p (f (ant_at p w) v) w }
        where w = world g

aw :: (World -> World) -> Game -> Game
aw f g = g { world = f w }
        where w = world g

qw :: Game -> (World -> r) -> r
qw g f = f w
        where w = world g

-- Define the function step, that takes a world and ant id, returning
-- a new world with the identified ant moved on one step (page 12)

-- Reverse composition will allow the code to more closely match the task
-- pseudo code. If the function are World -> World then you can have:
--     f1 >>>
--     f2 $ w
(>>>) :: (a -> b) -> (b -> c) -> (a -> c)
f >>> g = g . f

step :: Game -> Integer -> Game
step g id =
    if qw g (ant_is_alive id)  then
        let
            w = world g
            p = find_ant id w
            a = ant_at p w
        in
            if resting a > 0
            then aa p set_resting ((resting a) - 1) $ g
            else case get_instruction (color a) (state a) g of
                Sense sensedir st1 st2 cond ->
                    let p' = sensed_cell p (direction a) sensedir
                        st = if cell_matches p' cond (color a) w then st1 else st2
                    in  aa p set_state st $ g
                Mark i st   ->
                    aw (set_marker_at p (color a) i) >>>
                    aa p set_state st $ g
                Unmark i st ->
                    aw (clear_marker_at p (color a) i) >>>
                    aa p set_state st $ g
                PickUp st1 st2 ->
                    if has_food a || food_at p w == 0 then
                        aa p set_state st2 $ g
                    else
                        aw (set_food_at p ((food_at p w) - 1)) >>>
                        aa p set_has_food True  >>>
                        aa p set_state st1 $ g
                Drop st ->
                    if has_food a then
                        aw (set_food_at p ((food_at p w) + 1)) >>>
                        aa p set_has_food False >>>
                        aa p set_state st $ g  -- TODO try and lift this out
                    else
                        aa p set_state st $ g  -- TODO try and lift this out
                Turn lr st ->
                    aa p set_direction (turn lr (direction a)) >>> 
                    aa p set_state st $ g
                Move st1 st2 ->
                    let newp = adjacent_cell p (direction a) in
                        if qw g (rocky newp) || qw g (some_ant_is_at newp) then
                            aa p set_state st2 g
                        else 
                            aw (clear_ant_at p) >>>
                            aw (set_ant_at newp a) >>>
                            aa newp set_state st1 >>>
                            aa newp set_resting 14 >>>
                            check_for_surrounded_ants newp $ g
                Flip n st1 st2 ->
                    let (r, g2) = randomint n g
                        st = if r == 0 then st1 else st2 in
                    aa p set_state st $ g2
    else
        -- Ant is dead don't do anything
        g
    
-- Define adjacent_ants, check_for_surrounded_ant_at and
-- check_for_surrounded_ants (page 10)

adjacent_ants :: Pos -> Color -> Game -> Integer
adjacent_ants p c g = sum [ 1::Integer | p' <- positions
                                 , some_ant_is_at p' w
                                 , let a = ant_at p' w
                                 , color a == c ]
  where positions = map (adjacent_cell p) [East .. NorthEast]
        w = world g

check_for_surrounded_ant_at :: Pos -> Game -> Game
check_for_surrounded_ant_at p g
  | killAnt = aw (kill_ant_at p) >>>
              aw (set_food_at p f) $ g
  | otherwise = g
  where w = world g
        killAnt = some_ant_is_at p w && (adjacent_ants p c g) >= 5
        c = other_color (color ant)
        f = 3 + food_at p w + (if has_food ant then 1 else 0)
        ant = ant_at p w

check_for_surrounded_ants :: Pos -> Game -> Game
check_for_surrounded_ants p g = foldr check_for_surrounded_ant_at g positions
  where positions = p : map (adjacent_cell p) [East .. NorthEast]

-- Define randomint (page 11)

randomint :: Integer -> Game -> (Integer, Game)
randomint i g = (r, g')
    where rs = random g
          r = (head rs) `mod` i
          g' = g { random = tail rs }

randomIntStream :: Integer -> [Integer]
randomIntStream = drop 4
          . map ((`mod` 16384) . (`div` 65536))
          . iterate ((`mod` (65536*16384)) . (1 +) . (22695477 *))
