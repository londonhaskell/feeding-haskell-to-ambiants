module World where

import           Prelude hiding (id)
import qualified Data.Map as M

data Pos = Pos Int Int
           deriving (Show, Eq, Ord)

data Dir = East
         | SouthEast
         | SouthWest
         | West
         | NorthWest
         | NortEast
         deriving (Enum, Show, Eq, Ord, Read)

data Color = Red
           | Black
           deriving (Show, Eq, Read)

other_color :: Color -> Color
other_color Red   = Black
other_color Black = Red

type BrainState = Integer
mkBrainState :: Integer -> BrainState
mkBrainState i
  | i >= 0 && i <= 9999 = i
mkBrainState n          = 0

-- Create the Ant data type (line 8 of section 2.2, p4) and the 
-- accessor and update functions (line 3 of p5, section 2.2)

data Ant = Ant { id :: Integer
               , color :: Color
               , brainState :: BrainState
               , resting :: Integer
               , dir :: Dir
               , hasFood :: Bool
               }
         deriving (Show, Eq, Read)

set_state :: Ant -> Integer -> Ant
set_state a i = a { brainState = mkBrainState i }

set_resting :: Ant -> Integer -> Ant
set_resting a i = a { resting = i }

set_direction :: Ant -> Dir -> Ant
set_direction a d = a { dir = d }

set_has_food :: Ant -> Bool -> Ant
set_has_food a b = a { hasFood = b }


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

type Marker = Integer
mkMarker :: Integer -> BrainState
mkMarker i
  | i >= 0 && i <= 5 = i
mkMarker n           = 0

type FoodParticles = Integer
mkFoodParticles :: Integer -> Integer
mkFoodParticles i
  | i >= 0        = i
mkFoodParticles n = 0

data Cell = Rocky
          | Clear { ant           :: Maybe Ant
                  , foodParticles :: FoodParticles
                  , marker        :: Marker
                  }
          deriving (Show, Eq)

data World = World (M.Map Pos Cell)

rocky :: World -> Pos -> Bool
rocky (World w) p = case w M.! p of
                    Rocky -> True
                    _     -> False

-- Define some_ant_is_at, ant_at and set_ant_at functions (line 14 onwards of 
-- section 2.3, p5)

some_ant_is_at :: World -> Pos -> Bool
some_ant_is_at (World w) p = case w M.! p of
                             Rocky             -> False
                             Clear Nothing _ _ -> False
                             Clear _       _ _ -> True

ant_at :: World -> Pos -> Ant
ant_at (World w) p = case w M.! p of
    Clear (Just a) _ _ -> a
    otherwise          -> error ("Invalid call to ant_at: <world map> at pos: " ++ show p)

set_ant_at :: World -> Pos -> Ant -> World
set_ant_at x@(World w) p a = case w M.! p of
                           Clear Nothing  f m -> World $ M.insert p (Clear (Just a) f m) w
                           Clear (Just b) f m -> World $ M.insert p (Clear (Just a) f m) w
                           Rocky              -> error ("Invalid: you can't set an ant on a Rocky cell")

                                                             

-- Define ant_is_alive and find_ant functions (line 25 onwards of section 2.3, p5)

ant_is_alive :: World -> Integer -> Bool
ant_is_alive (World w) antId = undefined

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
