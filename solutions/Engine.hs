module Engine where

import World
import Data.Map (fromList, (!))

adjacent_cell :: Pos -> Dir -> Pos
adjacent_cell (x, y) East      = (x+1, y)
adjacent_cell (x, y) SouthEast = if even y then (x, y+1) else (x+1, y+1)
adjacent_cell (x, y) SouthWest = if even y then (x-1, y+1) else (x, y+1)
adjacent_cell (x, y) West      = (x-1, y)
adjacent_cell (x, y) NorthWest = if even y then (x-1, y-1) else (x, y-1)
adjacent_cell (x, y) NorthEast = if even y then (x, y-1) else (x+1, y-1)

-- Define left_or_right data type (section 2.1, page 3)

data Left_Or_Right = TurnLeft | TurnRight

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

-- Define sense_dir data type (section 2.1, page 4)

data Sense_Dir =
     Here       -- sense the ant’s current cell
   | Ahead      -- sense the cell straight ahead in the direction ant is facing
   | LeftAhead  -- sense the cell that would be ahead if ant turned left
   | RightAhead -- sense the cell that would be ahead if ant turned right
   
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
anthill_at = undefined

-- Define condition data type (section 2.6, page 7)

data Condition =
      Friend            -- cell contains an ant of the same color
    | Foe               -- cell contains an ant of the other color
    | FriendWithFood    -- cell contains an ant of the same color carrying food
    | FoeWithFood       -- cell contains an ant of the other color carrying food
    | Food              -- cell contains food (not being carried by an ant)
    | Rock              -- cell is rocky
    | Marker Marker     -- cell is marked with a marker of this ant's color
    | FoeMarker         -- cell is marked with *some* marker of the other color
    | Home              -- cell belongs to this ant's anthill
    | FoeHome           -- cell belongs to the other anthill
    deriving (Eq, Show)

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

-- Define State and Instruction data types (section 2.7, page 8)

type State = Integer

data Instruction = Sense Sense_Dir State State Condition
                 | Mark Marker State
                 | Unmark Marker State
                 | PickUp State State
                 | Drop State
                 | Turn Left_Or_Right State
                 | Move State State
                 | Flip Integer State State


-- Define get_instruction function (page 8)
-- TODO = Decide where the instructions are stored

get_instruction :: Color -> State -> World -> Instruction
get_instruction c s w = instructions ! (c, s) 
    where instructions = Data.Map.fromList ([] :: [((Color, State), Instruction)])

-- Define helper functions that will allow easy translation of the 
-- pseudo code for the step funciton.
-- set_state_pos     - Set the state of the ant at the given position
-- set_has_food_pos  - Set the food carried by the ant at the given position
-- set_direction_pos - Set the direction of the ant at the given position
-- set_resting_pos   - Set the resting count of the ant at the given position
    
set_state_pos :: Pos -> State -> World -> World
set_state_pos p st w = set_ant_at p (set_state (ant_at p w) st) w

set_has_food_pos :: Pos -> Bool -> World -> World
set_has_food_pos p b w = set_ant_at p (set_has_food (ant_at p w) b) w

set_direction_pos :: Pos -> Dir -> World -> World
set_direction_pos p d w = set_ant_at p (set_direction (ant_at p w) d) w

set_resting_pos :: Pos -> Integer -> World -> World
set_resting_pos p i w = set_ant_at p (set_resting (ant_at p w) i) w

-- Define the function step, that takes a world and ant id, returning
-- a new world with the identified ant moved on one step (page 12)

-- Reverse composition will allow the code to more closely match the task
-- pseudo code. If the function are World -> World then you can have:
--     f1 >>>
--     f2 $ w
(>>>) :: (a -> b) -> (b -> c) -> (a -> c)
f >>> g = g . f

step :: Integer -> World -> World
step id w =
    if (ant_is_alive id w) then
        let
            p = find_ant id w
            a = ant_at p w
        in
            if resting a > 0
            then set_ant_at p (set_resting a ((resting a) - 1)) w
            else case get_instruction (color a) (state a) w of
                Sense sensedir st1 st2 cond ->
                    let p' = sensed_cell p (direction a) sensedir
                        st = if cell_matches p' cond (color a) w then st1 else st2
                    in  set_state_pos p st $ w
                Mark i st   ->
                    set_marker_at p (color a) i >>>
                    set_state_pos p st $ w
                Unmark i st ->
                    clear_marker_at p (color a) i >>>
                    set_state_pos p st $ w
                PickUp st1 st2 ->
                    if has_food a || food_at p w == 0 then
                        set_state_pos p st2 $ w
                    else
                        set_food_at p ((food_at p w) - 1) >>>
                        set_has_food_pos p True  >>>
                        set_state_pos p st1 $ w
                Drop st ->
                    if has_food a then
                        set_food_at p ((food_at p w) + 1) >>>
                        set_has_food_pos p False >>>
                        set_state_pos p st $ w  -- TODO try and lift this out
                    else
                        set_state_pos p st $ w
                Turn lr st ->
                    set_direction_pos p (turn lr (direction a)) >>> 
                    set_state_pos p st $ w
                Move st1 st2 ->
                    let newp = adjacent_cell p (direction a) in
                        if rocky newp w || some_ant_is_at newp w then
                            set_state_pos p st2 w
                        else 
                            clear_ant_at p >>>
                            set_ant_at newp a >>>
                            set_state_pos p st1 >>>
                            set_resting_pos p 14 >>>
                            check_for_surrounded_ants newp $ w
                Flip n st1 st2 ->
                    let st = if randomint(n) == 0 then st1 else st2 in
                    set_state_pos p st $ w
    else
        -- Ant is dead don't do anthing
        w
    
-- Define adjacent_ants, check_for_surrounded_ant_at and
-- check_for_surrounded_ants (page 10)

adjacent_ants :: Pos -> Color -> World -> Integer
adjacent_ants = undefined

check_for_surrounded_ant_at :: Pos -> World -> World
check_for_surrounded_ant_at = undefined

check_for_surrounded_ants :: Pos -> World -> World
check_for_surrounded_ants = undefined

-- Define randomint (page 11)

randomint :: Integer -> Integer
randomint = undefined