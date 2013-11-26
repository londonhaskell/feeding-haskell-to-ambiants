{-# LANGUAGE ViewPatterns #-}

module Geography
    (
      Particles(..)
    , Markers(..)
    , Cell(..)
    , rocky
    , someAntIsAt
    , antAt
    , setAntAt
    , clearAntAt
    , antIsAlive
    , findAnt
    , killAntAt
    , foodAt
    ) where

import           Prelude  hiding (id)
import qualified Data.Map as M
import           Geometry
import           Biology

data Particles = Particles Int
               deriving (Show, Eq, Read)

data Markers = Markers (Color, Color)
             deriving (Show, Eq, Read)

data Cell = Rocky
          | Cell {
                   ant :: Maybe Ant
                 , foodParticles :: Particles
                 , markers :: Markers
                 }
          deriving (Show, Eq, Read)

type Cells = M.Map Pos Cell
type Ants  = M.Map Int Ant

data World = World Cells Ants

rocky :: World -> Pos -> Bool
rocky (World c a) p = case c M.! p of
                      Rocky -> True
                      _     -> False

someAntIsAt :: World -> Pos -> Bool
someAntIsAt (World c a) p = case c M.! p of
                              Cell (Just x) _ _ -> True
                              _                 -> False

-- Can only be called if someAntIsAt returns true
antAt :: World -> Pos -> Ant
antAt (World c a) p = case c M.! p of
                        Cell (Just x) _ _ -> x
                        _                 -> error("antAt called when no ant present: " ++ show p)

setAntAt :: World -> Pos -> Ant -> World
setAntAt (World c a) p x = let adjustCell (Cell _ f m) = Cell (Just x) f m
                           in  World (M.adjust adjustCell p      c)
                                     (M.adjust (const x)  (id x) a)

clearAntAt :: World -> Pos -> World
clearAntAt (World c a) p = case c M.! p of
                             Cell (Just x) f m -> World (M.insert p (Cell Nothing f m) c)
                                                        (M.delete (id x) a)
                             _          -> error("Can't clear an ant on a Rock: " ++ show p)

antIsAlive :: World -> Int -> Bool
antIsAlive (World c a) i = M.member i a

-- Can only be called if antIsAlive returns true
findAnt :: World -> Int -> Ant
findAnt (World c a) i = case M.lookup i a of
                          Just x -> x
                          _      -> error("findAnt called when ant not present (id): " ++ show i)

killAntAt :: World -> Pos -> World
killAntAt = clearAntAt

foodAt :: World -> Pos -> Int
foodAt (World c a) p = case c M.! p of
                         Cell _ (Particles i) _ -> i
                         _                      -> error("There are no food particles on a Rock: " ++ show p)
