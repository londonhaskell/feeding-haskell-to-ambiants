module Geography
    (
      Particles(..)
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
    , setMarkerAt
    , clearMarkerAt
    ) where

import           Prelude  hiding (id)
import qualified Data.Map as M
import           Geometry
import           Biology
import           Chemistry

data Particles = Particles Int
               deriving (Show, Eq, Read)

data Cell = Rocky
          | Clear {
                    ant           :: Maybe Ant
                  , foodParticles :: Particles
                  , redMarkers    :: Marker
                  , blackMarkers  :: Marker
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
                              Clear (Just x) _ _ _ -> True
                              _                    -> False

-- Can only be called if someAntIsAt returns true
antAt :: World -> Pos -> Ant
antAt (World c a) p = case c M.! p of
                        Clear (Just x) _ _ _ -> x
                        _                    -> error("antAt called when no ant present: " ++ show p)

setAntAt :: World -> Pos -> Ant -> World
setAntAt (World c a) p x = let adjustCell (Clear _ f r b) = Clear (Just x) f r b
                           in  World (M.adjust adjustCell p      c)
                                     (M.adjust (const x)  (id x) a)

clearAntAt :: World -> Pos -> World
clearAntAt (World c a) p = case c M.! p of
                             Clear (Just x) f r b -> World (M.insert p (Clear Nothing f r b) c)
                                                           (M.delete (id x) a)
                             _                    -> error("Can't clear an ant on a Rock: " ++ show p)

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
                         Clear _ (Particles i) _ _ -> i
                         _                         -> error("There are no food particles on a Rock: " ++ show p)

setMarkerAt :: World -> Pos -> Color -> Int -> World
setMarkerAt = adjustMarkerAt setMarker

clearMarkerAt :: World -> Pos -> Color -> Int -> World
clearMarkerAt = adjustMarkerAt clearMarker

adjustMarkerAt :: (Marker -> Int -> Marker) -> World -> Pos -> Color -> Int -> World
adjustMarkerAt func (World c a) p color i = let adjustCell (Clear x f r b) = case color of
                                                                               Red   -> Clear x f (func r i) b
                                                                               Black -> Clear x f r (func b i)
                                            in  World (M.adjust adjustCell p c)
                                                      a

checkMarkerAt :: World -> Pos -> Color -> Int -> Bool
checkMarkerAt (World c _) p Red   i = case c M.! p of
                                        Clear _ _ r _ -> checkMarker r i
                                        _             -> error("No markers at: " ++ show p)
checkMarkerAt (World c _) p Black i = case c M.! p of
                                        Clear _ _ _ b -> checkMarker b i
                                        _             -> error("No markers at: " ++ show p)

checkAnyMarkerAt :: World -> Pos -> Color -> Bool
checkAnyMarkerAt (World c _) p Red    = case c M.! p of
                                          Clear _ _ r _ -> checkAnyMarker r
                                          _             -> error("No markers at: " ++ show p)
checkAnyMarkerAt (World c _) p Black  = case c M.! p of
                                          Clear _ _ _ b -> checkAnyMarker b
                                          _             -> error("No markers at: " ++ show p)
