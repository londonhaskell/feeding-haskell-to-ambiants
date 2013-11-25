module Biology
    (
      Color
    , State
    , Ant
    , mkState
    , otherColor
    ) where

import Geometry

data Color = Red
           | Black
           deriving (Enum, Ord, Show, Eq, Read)

data State = State Int
           deriving (Show, Eq, Read)

data Ant = Ant {
                 id         :: Int
               , color      :: Color
               , state      :: State
               , resting    :: Int
               , currentDir :: Dir
               , hasFood    :: Bool
               }
         deriving (Show, Eq, Read)

mkState :: Int -> State
mkState i
    | i >= 0 && i <= 9999 = State i
    | otherwise           = error("mkState: int not in correct range: " ++ show i)

otherColor :: Color -> Color
otherColor Red   = Black
otherColor Black = Red
