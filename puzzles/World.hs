module World where

-- Create the Pos and Dir data types (Section 2.1, p3 lines 2 and 5)

-- Pos type ...
data Pos = ToDoPos  -- You might want to change this from a data declaration

-- Dir type ...
data Dir = ToDoDir

-- Create the Color data type and other_color function (line 2 and 4 of
-- Section 2.2, p4)

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

-- Define a default ant that can be used / modified when creating the world

defaultAnt :: Ant
defaultAnt = undefined

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

-- World data type
data World = ToDoWorld

-- A blank World value of the given size that can be adjusted
defaultWorld :: Integer -> Integer -> World
defaultWorld sizeX sizeY = undeinfed

-- A Cell can be rocky, or potentially contain an ant or other contents
data Cell = ToDoCell

-- Default cell. An empty cell
defaultCell :: Cell
defaultCell = undefined

-- The contents of the cells (other than the ants)
data CellContents = undefined

-- Default CellContents value
defaultCellContents :: CellContents
defaultCellContents = undefined

-- Helper function to get the CellContents from a World value
getCellContents :: Pos -> World -> CellContents
getCellContents = undefined

-- Helper function to adjust the CellContents in a World value.
adjustCellContents :: (CellContents -> CellContents) -> Pos -> World -> World
adjustCellContents = undefined

-- Write rocky function (line 3 of section 2.3, p5) and a function
-- to set a cell as rocky cell

rocky :: Pos -> World -> Bool
rocky = undefined

set_rocky :: Pos -> World -> World
set_rocky = undefined

-- Define some_ant_is_at, ant_at, set_ant_at and clear_ant_at functions
-- (line 14 onwards of section 2.3, p5)

some_ant_is_at :: Pos -> World -> Bool
some_ant_is_at = undefined

ant_at :: Pos -> World -> Ant
ant_at = undefined

set_ant_at :: Pos -> Ant -> World -> World
set_ant_at  = undefined
                                                             
clear_ant_at :: Pos -> World -> World
clear_ant_at = undefined

-- Define ant_is_alive and find_ant functions (line 25 onwards of section 2.3, p5)

ant_is_alive :: Integer -> World -> Bool
ant_is_alive = undefined

find_ant :: Integer -> World -> Pos
find_ant = undefined

-- Define food_at and set_food_at functions (lines 4 and 6 of p6, section 2.3)

food_at :: Pos -> World -> Integer
food_at = undefined

set_food_at :: Pos -> Integer -> World -> World
set_food_at = undefined

-- Define types for Marker and Markers (a group with 1 of each marker)

data Marker = ToDoMarker

data Markers = ToDoMarkers

-- Define default group of markers

defaultMarkers :: Markers
defaultMarkers = undefined

-- Define Marker functions

set_marker_at :: Pos -> Color -> Marker -> World -> World
set_marker_at = undefined

clear_marker_at :: Pos -> Color -> Marker -> World -> World
clear_marker_at = undefined

check_marker_at :: Pos -> Color -> Marker -> World -> Bool
check_marker_at = undefined

check_any_marker_at :: Pos -> Color -> World -> Bool
check_any_marker_at = undefined

-- Define parser from String to World (section 2.4, p6)

parseWorld :: String -> World
parseWorld s = undefined

-- Define printWorld function such that (printWorld . parseWorld) = id

printWorld :: World -> String
printWorld w = undefined

-- Further exercise:
-- Many of the functions have the form World -> Something -> World
-- which would appears similar to the State Monad.
-- Define a Monad instance to tidy up the code
