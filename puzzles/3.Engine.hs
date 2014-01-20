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
kill_ant_at :: World -> Pos -> World
kill_ant_at = undefined

-- Define anthill_at function (section 2.3, page 6)
anthill_at :: World -> Pos -> Color -> Bool
anthill_at = undefined

-- Define condition data type (section 2.6, page 7)
data Condition = ToDo_Condition

-- Define cell_matches function (section 2.6, page 7 and 8)
cell_matches :: World -> Pos -> Condition -> Color -> Bool
cell_matches = undefined

-- Define State and Instruction data types (section 2.7, page 8)
data State = ToDo_State

data Instruction = ToDo_Instruction

-- Define get_instruction function (page 8)
-- TODO = Decide where the instructions are stored
get_instruction :: Color -> State -> Instruction
get_instruction = undefined

-- Define the function step, that takes a world and ant id, returning
-- a new world with the identified ant moved on one step (page 12)
step :: World -> Integer -> World
step = undefined
    
-- Define adjacent_ants, check_for_surrounded_ant_at and
-- check_for_surrounded_ants (page 10)

-- Define randomint (page 11)
