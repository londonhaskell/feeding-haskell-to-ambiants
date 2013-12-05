module Geography
    (
      Particles(..)
    , Cell(..)
    , World
    , mkWorld
    , rocky
    , someAntIsAt
    , antAt
    , setAntAt
    , clearAntAt
    , antIsAlive
    , findAnt
    , killAntAt
    , foodAt
    , anthillAt
    , setMarkerAt
    , clearMarkerAt
    ) where

import           Prelude  hiding (id)
import qualified Data.Map as M
import           Geometry
import           Biology
import           Chemistry
import           Phenomenology

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

type Cells     = M.Map Pos Cell
type Ants      = M.Map Int Ant
type RedHill   = M.Map Pos Bool
type BlackHill = M.Map Pos Bool

data World = World {
                     cells     :: Cells
                   , ants      :: Ants
                   , redHill   :: RedHill
                   , blackHill :: BlackHill
                   }
           deriving (Show, Eq, Read)

mkWorld :: World
mkWorld  = World {
                   cells     = M.empty
                 , ants      = M.empty
                 , redHill   = M.empty
                 , blackHill = M.empty
                 }

rocky :: World -> Pos -> Bool
rocky w p = case (cells w) M.! p of
              Rocky -> True
              _     -> False

someAntIsAt :: World -> Pos -> Bool
someAntIsAt w p = case (cells w) M.! p of
                    Clear (Just x) _ _ _ -> True
                    _                    -> False

-- Can only be called if someAntIsAt returns true
antAt :: World -> Pos -> Ant
antAt w p = case (cells w) M.! p of
              Clear (Just x) _ _ _ -> x
              _                    -> error("antAt called when no ant present: " ++ show p)

setAntAt :: World -> Pos -> Ant -> World
setAntAt w p x = let adjustCell y = y { ant = Just x }
                     c = M.adjust adjustCell p (cells w)
                     a = M.adjust (const x) (id x) (ants w)
                 in  w { cells = c, ants = a }

clearAntAt :: World -> Pos -> World
clearAntAt w p = case (cells w) M.! p of
                   c@(Clear (Just x) _ _ _) -> w { cells = M.insert p (c { ant = Nothing }) (cells w)
                                                 , ants  = M.delete (id x) (ants w)
                                                 }
                   c@(Clear _        _ _ _) -> w { cells = M.insert p (c { ant = Nothing }) (cells w) }
                   _                        -> error("Can't clear an ant on a Rock: " ++ show p)

antIsAlive :: World -> Int -> Bool
antIsAlive w i = M.member i $ ants w

-- Can only be called if antIsAlive returns true
findAnt :: World -> Int -> Ant
findAnt w i = case M.lookup i (ants w) of
                Just x -> x
                _      -> error("findAnt called when ant not present (id): " ++ show i)

killAntAt :: World -> Pos -> World
killAntAt = clearAntAt

foodAt :: World -> Pos -> Int
foodAt w p = case (cells w) M.! p of
               Clear _ (Particles i) _ _ -> i
               _                         -> error("There are no food particles on a Rock: " ++ show p)

anthillAt :: World -> Pos -> Color -> Bool
anthillAt w p Red   = M.member p $ redHill   w
anthillAt w p Black = M.member p $ blackHill w

setMarkerAt :: World -> Pos -> Color -> Int -> World
setMarkerAt = adjustMarkerAt setMarker

clearMarkerAt :: World -> Pos -> Color -> Int -> World
clearMarkerAt = adjustMarkerAt clearMarker

adjustMarkerAt :: (Marker -> Int -> Marker)
               -> World -> Pos -> Color -> Int -> World
adjustMarkerAt func w p color i =
    let adjustCell x = case color of
                         Red   -> x { redMarkers   = func (redMarkers x)   i }
                         Black -> x { blackMarkers = func (blackMarkers x) i }
        c = M.adjust adjustCell p (cells w)
    in  w { cells = c }

checkMarkerAt :: World -> Pos -> Color -> Int -> Bool
checkMarkerAt w p Red   i = case (cells w) M.! p of
                              Clear _ _ r _ -> checkMarker r i
                              _             -> error("No markers at: " ++ show p)
checkMarkerAt w p Black i = case (cells w) M.! p of
                              Clear _ _ _ b -> checkMarker b i
                              _             -> error("No markers at: " ++ show p)

checkAnyMarkerAt :: World -> Pos -> Color -> Bool
checkAnyMarkerAt w p Red    = case (cells w) M.! p of
                                Clear _ _ r _ -> checkAnyMarker r
                                _             -> error("No markers at: " ++ show p)
checkAnyMarkerAt w p Black  = case (cells w) M.! p of
                                Clear _ _ _ b -> checkAnyMarker b
                                _             -> error("No markers at: " ++ show p)

cellMatches :: World -> Pos -> Condition -> Color -> Bool
cellMatches w p cond           _
  | rocky w p                    = cond == Rock
cellMatches _ _ Rock           _ = False
cellMatches w p Friend         c = someAntIsAt w p &&
                                   (color $ antAt w p) == c
cellMatches w p Foe            c = someAntIsAt w p &&
                                   (color $ antAt w p) /= c
cellMatches w p FriendWithFood c = someAntIsAt w p &&
                                   (color $ antAt w p) == c &&
                                   (hasFood $ antAt w p)
cellMatches w p FoeWithFood    c = someAntIsAt w p &&
                                   (color $ antAt w p) /= c &&
                                   (hasFood $ antAt w p)
cellMatches w p (Marker i)     c = checkMarkerAt w p c i
cellMatches w p FoeMarker      c = checkAnyMarkerAt w p $ otherColor c
cellMatches w p Home           c = anthillAt w p c
cellMatches w p FoeHome        c = anthillAt w p $ otherColor c
cellMatches w p Food           _ = foodAt w p > 0


adjacentAnts :: World -> Pos -> Color -> Int
adjacentAnts w pos c =
    let positions     = map (adjacentCell pos) [East .. NorthEast]
        go acc []     = acc
        go acc (p:ps) = if   (someAntIsAt w p) && color (antAt w p) == c
                        then go (acc + 1) ps
                        else go acc ps
    in  go 0 positions

