module World where

-- Create the Pos and Dir data types (Section 2.1, p3 lines 2 and 5)

-- Pos type ...
data Pos = ToDoPos  -- You might want to change this from a data declaration

-- Dir type ...
data Dir = ToDoDir


-- Create the Color data type and other_color function (line 2 and 4 of Section 2.2, p4)

data Color = ToDoColour

other_color :: Color -> Color
other_color = undefined

-- Create the Ant data type (line 8 of section 2.2, p4) and the 
-- accessor and update functions (line 3 of p5, section 2.2)

data Ant = ToDoAnt

set_state :: Ant -> Integer -> Ant
set_state = undefined

set_resting :: Ant -> Integer -> Ant
set_resting = undefined

set_direction :: Ant -> Dir -> Ant
set_direction = undefined

set_has_food :: Ant -> Bool -> Ant
set_has_food = undefined


-- Define a Cell data structure that records all the 
-- information for one cell and a World data structure 
-- that holds all the cells in the game board (p5, section 
-- 2.3).
--
-- The details of the Marker data type are on p7 (second 
-- line of section 2.5)
--
-- There are some design choices to make with both data
-- type. Read the rest of the functions required to make
-- sure it can easily support the functions needed.

-- World, Cell, Marker type...

data World = ToDoWorld

-- Write rocky function (line 3 of section 2.3, p5)

rocky :: World -> Pos -> Bool
rocky = undefined

-- Define some_ant_is_at, ant_at and set_ant_at functions (line 14 onwards of 
-- section 2.3, p5)

some_ant_is_at :: World -> Pos -> Bool
some_ant_is_at = undefined

ant_at :: World -> Pos -> Ant
ant_at = undefined

set_ant_at :: World -> Pos -> Ant -> World
set_ant_at  = undefined
                                                             

-- Define ant_is_alive and find_ant functions (line 25 onwards of section 2.3, p5)

ant_is_alive :: World -> Integer -> Bool
ant_is_alive = undefined

find_ant :: World -> Integer -> Pos
find_ant = undefined


-- Define food_at and set_food_at functions (lines 4 and 6 of p6, section 2.3)

food_at :: World -> Pos -> Integer
food_at = undefined

set_food_at :: World -> Pos -> Integer -> World
set_food_at = undefined

-- Define parser from String to World (section 2.4, p6)

parseWorld :: String -> World
parseWorld s = undefined

-- Define printWorld function such that (parseWorld . printWorld) = id

printWorld :: World -> String
printWorld w = undefined

-- Futher exercise:
-- Many of the functions have the form World -> Something -> World
-- which would appears similar to the State Monad.
-- Define a Monad instance to tidy up the code