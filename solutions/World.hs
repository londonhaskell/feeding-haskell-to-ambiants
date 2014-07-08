module World where

import Prelude hiding ( id )
import Data.Map ( Map, (!), fromList, toList, adjust, insert, member, 
                  empty, keys, elems, delete )

-- Create the Pos and Dir data types (Section 2.1, p3 lines 2 and 5)

-- Pos type
type Pos = (Integer, Integer)

-- Dir type
data Dir = East | SouthEast | SouthWest
         | West | NorthWest | NorthEast
    deriving (Eq, Enum, Ord, Show, Read)

-- Define left_or_right data type (section 2.1, page 3)

data Left_Or_Right = TurnLeft | TurnRight
    deriving (Show)
    
-- Create the Color data type and other_color function (line 2 and 4 of
-- Section 2.2, p4)

data Color = Red | Black
    deriving (Eq, Show, Read, Ord)

other_color :: Color -> Color
other_color Red   = Black
other_color Black = Red

-- Create the Ant data type (line 8 of section 2.2, p4) and the 
-- accessor and update functions (line 3 of p5, section 2.2)

data Ant = Ant { id :: Integer
               , color :: Color
               , state :: Integer
               , resting :: Integer
               , direction :: Dir
               , has_food :: Bool
               }
    deriving (Show, Eq, Read)

set_state :: Ant -> Integer -> Ant
set_state a i = a { state = i }

set_resting :: Ant -> Integer -> Ant
set_resting a i = a { resting = i }

set_direction :: Ant -> Dir -> Ant
set_direction a d = a { direction = d }

set_has_food :: Ant -> Bool -> Ant
set_has_food a b = a { has_food = b }

-- Define a default ant that can be used / modified when creating the world
defaultAnt :: Ant
defaultAnt = Ant { id = 0
                 , color = Red
                 , state = 0
                 , resting = 0
                 , direction = East
                 , has_food = False
                 }

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
data World = World { cells :: Map Pos Cell
                   , antPositions :: Map Integer Pos
                   , hills :: Map Pos Color
                   }
   deriving (Show)

-- A blank World value of the given size that can be adjusted
defaultWorld :: Integer -> Integer -> World
defaultWorld sizeX sizeY = World { cells = fromList defaultCellsList
                                 , antPositions = empty
                                 , hills = empty
                                 }
    where defaultCellsList = [ ( (x,y), defaultCell ) | x <- [ 0 .. sizeX - 1]
                                                      , y <- [ 0 .. sizeY - 1]
                                                      ]

-- A Cell can be rocky, or potentially contain an ant or other contents
data Cell = Rocky
          | ClearCell CellContents
          | AntCell Ant CellContents
    deriving (Show)

-- Default cell. An empty cell    
defaultCell :: Cell
defaultCell = ClearCell defaultCellContents

-- The contents of the cells (other than the ants)
data CellContents = CellContents { food :: Integer
                                 , markers :: Markers
                                 }
    deriving (Show)

-- Default CellContents value
defaultCellContents :: CellContents
defaultCellContents = CellContents { food = 0
                                   , markers = defaultMarkers
                                   }

-- Helper function to get the CellContents from a World value
getCellContents :: Pos -> World -> CellContents
getCellContents p w = f ( (cells w) ! p )
    where f :: Cell -> CellContents
          f (ClearCell c) = c
          f (AntCell a c) = c
          f c = error ("getCellContents called on " ++ (show c)
                                ++ " at " ++ (show p))

-- Helper function to adjust the CellContents in a World value.
-- Will fail if called on a rocky cell
adjustCellContents :: (CellContents -> CellContents) -> Pos -> World -> World
adjustCellContents f p w = w { cells = adjust g p (cells w) }
    where g :: Cell -> Cell
          g (ClearCell c) = ClearCell (f c)
          g (AntCell a c) = AntCell a (f c)
          g c = error ("adjustCellContents called on " ++ (show c)
                                ++ " at " ++ (show p))

-- Write rocky function (line 3 of section 2.3, p5) and a function
-- to set a cell as rocky cell

rocky :: Pos -> World -> Bool
rocky p w = f ((cells w) ! p)
    where f Rocky = True
          f _     = False

set_rocky :: Pos -> World -> World
set_rocky p w = w { cells = insert p Rocky ( cells w ) }

-- Define some_ant_is_at, ant_at, set_ant_at and clear_ant_at functions
-- (line 14 onwards of section 2.3, p5)

some_ant_is_at :: Pos -> World -> Bool
some_ant_is_at p w = f ((cells w) ! p)
    where f (AntCell _ _) = True
          f  _            = False

ant_at :: Pos -> World -> Ant
ant_at p w = f ((cells w) ! p)
    where f (AntCell a _) = a
          f c = error ("ant_at called on " ++ (show c) ++ " at " ++ (show p))

setAntAndHill :: Pos -> Ant -> World -> World
setAntAndHill p a w = w' { hills = newHills }
    where w'       = set_ant_at p a w
          newHills = ( insert p (color a) (hills w') )

set_ant_at :: Pos -> Ant -> World -> World
set_ant_at p a w = (w { cells = newCells, antPositions = newAntPositions } )
    where newCells        = ( adjust f p (cells w) )
          newAntPositions = ( insert (id a) p (antPositions w) )
          f (AntCell _ c) = AntCell a c
          f (ClearCell c) = AntCell a c
          f c = error ("set_ant_at called on " ++ (show c) ++ " at " ++ (show p))

clear_ant_at :: Pos -> World -> World
clear_ant_at p w = w { cells = newCells, antPositions = newAntPositions }
    where newCells        = ( adjust f p (cells w) )
          newAntPositions = ( delete (id (ant_at p w)) (antPositions w) )
          f (AntCell _ c) = ClearCell c
          f c = error ("clear_ant_at called on " ++ (show c) ++ " at " ++ (show p))
          
-- Define ant_is_alive and find_ant functions (line 25 onwards of section 2.3, p5)

ant_is_alive :: Integer -> World -> Bool
ant_is_alive i w = member i (antPositions w)

find_ant :: Integer -> World -> Pos
find_ant i w = (antPositions w) ! i

-- Define food_at and set_food_at functions (lines 4 and 6 of p6, section 2.3)

food_at :: Pos -> World -> Integer
food_at p w = f ((cells w) ! p)
    where f (AntCell _ c) = food c
          f (ClearCell c) = food c
          f c = error ("food_at called on " ++ (show c) ++ " at " ++ (show p))

set_food_at :: Pos -> Integer -> World -> World
set_food_at p i w = adjustCellContents f p w
    where f c = c { food = i }

-- Define types for Marker and Markers (a group with 1 of each marker)

-- There are 6 types of marker, 0..5
data Marker = M0 | M1 | M2 | M3 | M4 | M5
    deriving (Read, Show, Eq, Ord, Bounded, Enum)

mkMarker :: Integer -> Marker
mkMarker n | 0 <= n && n <= 5 = [M0, M1, M2, M3, M4, M4, M5 ] !! (fromInteger n)
           | otherwise        = error ("markMarker outside bounds: " ++ (show n))

-- The markers for both colours will be stored together
type Markers = Map (Color, Marker) Bool
    
-- Define default group of markers

defaultMarkers :: Markers
defaultMarkers = fromList [ ((c, m), b) | c <- [ Red, Black]
                                        , m <- [ M0 .. M5 ]
                                        , b <- [ False ]
                                        ]

-- Define Marker functions

set_marker_at :: Pos -> Color -> Marker -> World -> World
set_marker_at p c m w = adjustCellContents (f c m) p w
    where f col m cc = cc { markers = insert (col, m) True (markers cc) }

clear_marker_at :: Pos -> Color -> Marker -> World -> World
clear_marker_at p c m w = adjustCellContents (f c m) p w
    where f col m cc = cc {  markers = insert (col, m) False (markers cc) }

check_marker_at :: Pos -> Color -> Marker -> World -> Bool
check_marker_at p col m w = (markers (getCellContents p w)) ! (col, m)

check_any_marker_at :: Pos -> Color -> World -> Bool
check_any_marker_at p c w = not . Prelude.null
                                . filter (\((c', _), _) -> c' == c)  -- filter by colour
                                . filter ( snd )  -- only the markers that are set
                                . toList . markers $ getCellContents p w

-- Define sense_dir data type (section 2.1, page 4)

data Sense_Dir =
     Here       -- sense the ant’s current cell
   | Ahead      -- sense the cell straight ahead in the direction ant is facing
   | LeftAhead  -- sense the cell that would be ahead if ant turned left
   | RightAhead -- sense the cell that would be ahead if ant turned right
   deriving (Read, Show, Eq)
   
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
    deriving (Eq, Read, Show)

-- Define InsState and Instruction data types (section 2.7, page 8)

type InsState = Integer

data Instruction = Sense Sense_Dir InsState InsState Condition
                 | Mark Marker InsState
                 | Unmark Marker InsState
                 | PickUp InsState InsState
                 | Drop InsState
                 | Turn Left_Or_Right InsState
                 | Move InsState InsState
                 | Flip Integer InsState InsState
    deriving (Show)

-- Define parser from String to World (section 2.4, p6)

parseWorld :: String -> World
parseWorld s = snd $ foldl applyToWorld (0,(defaultWorld sizeX sizeY)) pcs
    where
         -- First 2 lines are the dimensions and then the rest of the characters
         -- have white-space between them so can be treated as separate words
        (one:two:rest) = words s
        sizeX = (read one) :: Integer
        sizeY = (read two) :: Integer
        -- Get the positions (ps) by row and then by column
        ps = [ (x, y) | y <- [0 .. sizeY - 1], x <- [0 .. sizeX - 1] ]
        -- Build the positions and characters
        pcs = zip ps rest
        -- Take pairs of positions and characters and apply them to a World
        applyToWorld :: (Integer, World) -> (Pos, String) -> (Integer, World)
        applyToWorld (i, w) (p, "#") = (i, w { cells = insert p Rocky (cells w) })
        applyToWorld (i, w) (p, ".") = (i, w) -- Leave as default (ClearCell)
        applyToWorld (i, w) (p, "+") = (i+1, setAntAndHill p (defaultAnt { color = Red, id = i }) w)
        applyToWorld (i, w) (p, "-") = (i+1, setAntAndHill p (defaultAnt { color = Black, id = i }) w)
        applyToWorld (i, w) (p, n) | "0" <= n && n <= "9"  
                                     = (i, set_food_at p (read n :: Integer) w)
        applyToWorld (i, w) (p, s)   = error ("Cannot parse " ++ s ++ " at " ++ show p)

-- Define printWorld function such that (printWorld . parseWorld) = id

printWorld :: World -> String
printWorld w = unlines (show sizeX : show sizeY : boardLines)
    where
        sizeX = worldSizeX w
        sizeY = worldSizeY w
        -- For ps (the positions) we want the coordinates by row 
        -- then column, row 0, col 0, col 1, col 2 ... row 1, col 0 ...
        -- As this is a list of lists comprehension, the order of the 
        -- generators is the other way round to a noral list comprehension
        -- that would be in the same co-ordinate order
        ps = [ [ (x, y) | x <- [0 .. sizeX - 1] ] | y <- [0 .. sizeY - 1] ]
        -- Build a list of lists containing the position and cell
        boardCells :: [[(Pos, Cell)]] -- Sub lists are rows
        boardCells = map (map (\p -> (p, (cells w) ! p))) ps -- Keep list of lists
        -- Build each the board line by line
        boardLines :: [String] -- Each string is a row
        boardLines = map (concatMap convertAndPrefix) boardCells
        -- Convert each cell type to the correct character and add a space before
        -- if needed
        convertAndPrefix :: (Pos, Cell) -> String -- Add the correct spaces
        convertAndPrefix ((x, y), c) = (if x /= 0 || odd y then " " else "")
                                            ++ convertCell c
         -- Convert the Cells to 1 character strings
        convertCell :: Cell -> String
        convertCell Rocky         = "#"
        convertCell (ClearCell c) = if food c == 0 then "." else show (food c)
        convertCell (AntCell a _) = if color a == Red then "+" else "-"

worldSizeX w = (maximum (map fst ps)) + 1
    where
        ps = keys (cells w)

worldSizeY w = (maximum (map snd ps)) + 1
    where
        ps = keys (cells w)

-- Further exercise:
-- Many of the functions have the form World -> Something -> World
-- which would appears similar to the State Monad.
-- Define a Monad instance to tidy up the code
