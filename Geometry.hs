module Geometry where

import Prelude hiding (Left, Right)

data Pos = Pos Int Int
         deriving (Ord, Show, Eq, Read)

data Dir = East
         | SouthEast
         | SouthWest
         | West
         | NorthWest
         | NorthEast
         deriving (Enum, Ord, Show, Eq, Read)

data Turn = Left
          | Right
          deriving (Show, Eq, Read)

data SenseDir = Here
              | Ahead
              | LeftAhead
              | RightAhead
              deriving (Show, Eq, Read)

adjacentCell :: Pos -> Dir -> Pos
adjacentCell (Pos x y) East      = Pos (x+1) y
adjacentCell (Pos x y) SouthEast = Pos (if even y then x   else x+1) (y+1)
adjacentCell (Pos x y) SouthWest = Pos (if even y then x-1 else x) (y+1)
adjacentCell (Pos x y) West      = Pos (x-1) y
adjacentCell (Pos x y) NorthWest = Pos (if even y then x-1 else x) (y-1)
adjacentCell (Pos x y) NorthEast = Pos (if even y then x   else x+1) (y-1)

turn :: Turn -> Dir -> Dir
turn Left  d = toEnum $ (fromEnum d) + 5 `mod` 6
turn Right d = toEnum $ (fromEnum d) + 1 `mod` 6

sensedCell :: Pos -> Dir -> SenseDir -> Pos
sensedCell p d Here       = p
sensedCell p d Ahead      = adjacentCell p d
sensedCell p d LeftAhead  = adjacentCell p $ turn Left d
sensedCell p d RightAhead = adjacentCell p $ turn Right d
