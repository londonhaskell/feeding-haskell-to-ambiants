module World where

import Prelude hiding ( id )
import Data.Map ( Map, (!), fromList, adjust, insert, member, empty, keys )

type Pos = (Integer, Integer)

data Dir = East | SouthEast | SouthWest
         | West | NorthWest | NorthEast
    deriving (Eq, Enum, Ord, Show, Read)


data Color = Red | Black
    deriving (Eq, Show, Read)

other_color :: Color -> Color
other_color Red   = Black
other_color Black = Red

data Ant = Ant { id :: Integer
               , color :: Color
               , state :: Integer
               , resting :: Integer
               , direction :: Dir
               , has_food :: Bool
               }
    deriving (Show, Eq, Read)

defaultAnt = Ant { id = 0
                 , color = Red
                 , state = 0
                 , resting = 0
                 , direction = East
                 , has_food = False
                 }

set_state :: Ant -> Integer -> Ant
set_state a i = a { state = i }

set_resting :: Ant -> Integer -> Ant
set_resting a i = a { resting = i }

set_direction :: Ant -> Dir -> Ant
set_direction a d = a { direction = d }

set_has_food :: Ant -> Bool -> Ant
set_has_food a b = a { has_food = b }

data World = World { cells :: Map Pos Cell
                   , antPositions :: Map Integer Pos
                   }
   deriving (Show)

mkWorld sizeX sizeY = World { cells = fromList defaultCellsList
                            , antPositions = empty
                            }
    where defaultCellsList = [ ( (x,y), defaultCell ) | x <- [ 0 .. sizeX - 1]
                                                      , y <- [ 0 .. sizeY - 1]
                                                      ]

-- createWorld :: Pos -> World
-- createWorld (sizeX, sizeY) = World { cells = fromList posCellList
                           -- , antPositions = empty
                           -- }
    -- where posCellList = [ ( (x,y), defaultCell )  | x <- [ 0 .. sizeX - 1]
                                                  -- ,  ]

adjustCellContents :: World -> (CellContents -> CellContents) -> Pos -> World
adjustCellContents w f p = w { cells = adjust g p (cells w) }
    where g :: Cell -> Cell
          g (ClearCell c) = ClearCell (f c)
          g (AntCell a c) = AntCell a (f c)
          g c = error ("adjustCellContents called on " ++ (show c)
                                ++ " at " ++ (show p))

type Marker = ( Bool, Bool, Bool, Bool, Bool, Bool )

defaultMarker :: Marker
defaultMarker = (False, False, False, False, False, False)

data CellContents = CellContents { food :: Integer
                                 , redMarker :: Marker
                                 , blackMarker :: Marker
                                 }
    deriving (Show)

defaultCellContents :: CellContents
defaultCellContents = CellContents { food = 0
                      , redMarker = defaultMarker
                      , blackMarker = defaultMarker
                      }
                  
data Cell = Rocky
          | ClearCell CellContents
          | AntCell Ant CellContents
    deriving (Show)
    
defaultCell :: Cell
defaultCell = ClearCell defaultCellContents

rocky :: World -> Pos -> Bool
rocky w p = f ((cells w) ! p)
    where f Rocky = True
          f _     = False

some_ant_is_at :: World -> Pos -> Bool
some_ant_is_at w p = f ((cells w) ! p)
    where f (AntCell _ _) = True
          f  _            = False

ant_at :: World -> Pos -> Ant
ant_at w p = f ((cells w) ! p)
    where f (AntCell a _) = a
          f c = error ("ant_at called on " ++ (show c) ++ " at " ++ (show p))

set_ant_at :: World -> Pos -> Ant -> World
set_ant_at w p a = w { cells = newCells, antPositions = newAntPositions }
    where newCells        = ( adjust f p (cells w) )
          newAntPositions = ( insert (id a) p (antPositions w) )
          f (AntCell _ c) = AntCell a c
          f (ClearCell c) = AntCell a c
          f c = error ("set_ant_at called on " ++ (show c) ++ " at " ++ (show p))

ant_is_alive :: World -> Integer -> Bool
ant_is_alive w i = member i (antPositions w)

find_ant :: World -> Integer -> Pos
find_ant w i = (antPositions w) ! i

food_at :: World -> Pos -> Integer
food_at w p = f ((cells w) ! p)
    where f (AntCell _ c) = food c
          f (ClearCell c) = food c
          f c = error ("food_at called on " ++ (show c) ++ " at " ++ (show p))

set_food_at :: World -> Pos -> Integer -> World
set_food_at w p i = adjustCellContents w f p
    where f c = c { food = i }

-- Define parser from String to World (section 2.4, p6)

parseWorld :: String -> World
parseWorld s = foldr applyToWorld (mkWorld sizeX sizeY) ps
    where
        (one:two:rest) = words s
        sizeX = (read one) :: Integer
        sizeY = (read two) :: Integer
        -- Get the positions (ps) by row and then by column
        ps = zip [ (x, y) | y <- [0 .. sizeY - 1], x <- [0 .. sizeX - 1] ] rest
        applyToWorld :: (Pos, String) -> World -> World
        applyToWorld (p, "#") w = w { cells = insert p Rocky (cells w) }
        applyToWorld (p, ".") w = w -- Leave as default (ClearCell)
        applyToWorld (p, "+") w = set_ant_at w p (defaultAnt { color = Red })
        applyToWorld (p, "-") w = set_ant_at w p (defaultAnt { color = Black })
        f (p, c)   w = set_food_at w p (read c :: Integer)

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
        boardCells :: [[(Pos, Cell)]] -- Sub lists are rows
        boardCells = map (map (\p -> (p, (cells w) ! p))) ps -- Keep list of lists
        boardLines :: [String] -- Each string is a row
        boardLines = map (concatMap convertAndPrefix) boardCells
        convertAndPrefix :: (Pos, Cell) -> String -- Add the correct spaces
        convertAndPrefix ((x, y), c) = (if x /= 0 || odd y then " " else "")
                                            ++ convertCell c
        convertCell :: Cell -> String -- Convert the Cells to 1 character strings
        convertCell Rocky         = "#"
        convertCell (ClearCell c) = if food c == 0 then "." else show (food c)
        convertCell (AntCell a _) = if color a == Red then "+" else "-"

worldSizeX w = (maximum (map fst ps)) + 1
    where
        ps = keys (cells w)

worldSizeY w = (maximum (map snd ps)) + 1
    where
        ps = keys (cells w)

-- Futher exercise:
-- Many of the functions have the form World -> Something -> World
-- which would appears similar to the State Monad.
-- Define a Monad instance to tidy up the code