module Engine where

import World

-- Define adjacent_cell from (section 2.1, page 3)
adjacent_cell :: Pos -> Dir -> Pos
adjacent_cell = undefined

-- Define left_or_right data type (section 2.1, page 3)

data Left_Or_Right = ToDo_Left_Or_Right

-- Define turn function which takes a direction to turn and
-- orientation direction and returns a new orientation
-- (section 2.1, page 4)
turn :: Left_Or_Right -> Dir -> Dir
turn = undefined

-- Define sense_dir data type (section 2.1, page 4)

data Sense_Dir = ToDo_Sense_Dir
   
-- Define sensed_cell function that takes a position, direction
-- and a direction to sense in and returns the position that will
-- be sensed (section 2.1, page 4) 
sensed_cell :: Pos -> Dir -> Sense_Dir -> Pos
sensed_cell = undefined

-- Define kill_ant_at function (section 2.3, page 5)

kill_ant_at :: Pos -> World -> World
kill_ant_at = undefined

-- Define anthill_at function (section 2.3, page 6)

anthill_at :: Pos -> Color -> World -> Bool
anthill_at = undefined

-- Define condition data type (section 2.6, page 7)

data Condition = ToDo_Condition

-- Define cell_matches function (section 2.6, page 7 and 8)

cell_matches :: Pos -> Condition -> Color -> World -> Bool
cell_matches = undefined

-- Define State and Instruction data types (section 2.7, page 8)

data State = ToDo_State

data Instruction = ToDo_Instruction

-- Define get_instruction function (page 8)
-- TODO = Decide where the instructions are stored

get_instruction :: Color -> State -> World -> Instruction
get_instruction = undefined

-- Define helper functions that will allow easy translation of the 
-- pseudo code for the step funciton.
-- set_state_pos     - Set the state of the ant at the given position
-- set_has_food_pos  - Set the food carried by the ant at the given position
-- set_direction_pos - Set the direction of the ant at the given position
-- set_resting_pos   - Set the resting count of the ant at the given position
    
set_state_pos :: Pos -> State -> World -> World
set_state_pos = undefined

set_has_food_pos :: Pos -> Bool -> World -> World
set_has_food_pos = undefined

set_direction_pos :: Pos -> Dir -> World -> World
set_direction_pos = undefined

set_resting_pos :: Pos -> Integer -> World -> World
set_resting_pos = undefined

-- Define the function step, that takes a world and ant id, returning
-- a new world with the identified ant moved on one step (page 12)

-- Reverse composition will allow the code to more closely match the task
-- pseudo code 
(>>>) :: (a -> b) -> (b -> c) -> (a -> c)
f >>> g = g . f

step :: Integer -> World -> World
step = undefined
    
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