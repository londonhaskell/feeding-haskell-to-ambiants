module Geography
    (
      Particles(..)
    , Cell(..)
    , World(..)
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
    , setFoodAt
    , anthillAt
    , setMarkerAt
    , clearMarkerAt
    , cellMatches
    , checkForSurroundedAnts
    , randomInt
    , parse
    ) where

import           Prelude  hiding (id)
import qualified Data.Map as M
import qualified Data.Char as C
import           Geometry
import           Biology
import           Chemistry
import           Phenomenology
import qualified NumberTheory as N

data Particles = Particles Int
               deriving (Show, Eq, Read)

type MarkerMap = M.Map Marker Bool

data Cell = Rocky
          | Clear {
                    ant           :: Maybe Ant
                  , foodParticles :: Particles
                  , redMarkers    :: MarkerMap
                  , blackMarkers  :: MarkerMap
                  }
          deriving (Show, Eq, Read)

emptyClearCell :: Cell
emptyClearCell = Clear Nothing (Particles 0) M.empty M.empty

type Cells     = M.Map Pos Cell
type Ants      = M.Map Int (Ant, Pos)
type RedHill   = M.Map Pos Bool
type BlackHill = M.Map Pos Bool

data World = World {
                     cells     :: Cells
                   , ants      :: Ants
                   , redHill   :: RedHill
                   , blackHill :: BlackHill
                   , random    :: [Int]
                   }
           deriving (Show, Eq, Read)

mkWorld :: Int -> World
mkWorld seed = World {
                   cells     = M.empty
                 , ants      = M.empty
                 , redHill   = M.empty
                 , blackHill = M.empty
                 , random    = N.randomInt seed
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
setAntAt w p a = let alterCell new old = old { ant = Just a }
                     alterAnt  new old = (a, p)
                     cs = M.insertWith alterCell p (emptyClearCell { ant = Just a }) (cells w)
                     as = M.insertWith alterAnt  (id a) (a, p) (ants w)
                 in  w { cells = cs, ants = as }

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
findAnt :: World -> Int -> Pos
findAnt w i = case M.lookup i (ants w) of
                Just (_, x) -> x
                _           -> error("findAnt called when ant not present (id): " ++ show i)

killAntAt :: World -> Pos -> World
killAntAt = clearAntAt

foodAt :: World -> Pos -> Int
foodAt w p = case (cells w) M.! p of
               Clear _ (Particles i) _ _ -> i
               _                         -> error("There are no food particles on a Rock: " ++ show p)

setFoodAt :: World -> Pos -> Int -> World
setFoodAt w p i = let food      = Particles i
                      newCell   = Clear Nothing food M.empty M.empty
                      f new old = old { foodParticles = food }
                      cs'       = M.insertWith f p newCell (cells w)
                  in  w { cells = cs' }

anthillAt :: World -> Pos -> Color -> Bool
anthillAt w p Red   = M.member p $ redHill   w
anthillAt w p Black = M.member p $ blackHill w

setMarker :: MarkerMap -> Int -> MarkerMap
setMarker m i = M.insert (mkMarker i) True m

setMarkerAt :: World -> Pos -> Color -> Int -> World
setMarkerAt = adjustMarkerAt setMarker

clearMarker :: MarkerMap -> Int -> MarkerMap
clearMarker m i = M.delete (mkMarker i) m

clearMarkerAt :: World -> Pos -> Color -> Int -> World
clearMarkerAt = adjustMarkerAt clearMarker

adjustMarkerAt :: (MarkerMap -> Int -> MarkerMap)
               -> World -> Pos -> Color -> Int -> World
adjustMarkerAt f w p color i =
    let adjustCell x = case color of
                         Red   -> x { redMarkers   = f (redMarkers x)   i }
                         Black -> x { blackMarkers = f (blackMarkers x) i }
        c = M.adjust adjustCell p (cells w)
    in  w { cells = c }

checkMarker :: MarkerMap -> Int -> Bool
checkMarker m i = M.member (mkMarker i) m

checkMarkerAt :: World -> Pos -> Color -> Int -> Bool
checkMarkerAt w p Red   i = case (cells w) M.! p of
                              Clear _ _ r _ -> checkMarker r i
                              _             -> error("No markers at: " ++ show p)
checkMarkerAt w p Black i = case (cells w) M.! p of
                              Clear _ _ _ b -> checkMarker b i
                              _             -> error("No markers at: " ++ show p)

checkAnyMarker :: MarkerMap -> Bool
checkAnyMarker m = not $ M.null m

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

-- Tidy this up!
checkForSurroundedAntAt :: World -> Pos -> World
checkForSurroundedAntAt w p =
    if   someAntIsAt w p
    then let a = antAt w p
         in  if adjacentAnts w p (otherColor (color a)) >= 5
             then setFoodAt (killAntAt w p) p (foodAt w p + 3 + (if hasFood a then 1 else 0))
             else w
    else w

checkForSurroundedAnts :: World -> Pos -> World
checkForSurroundedAnts w p =
    let w'    = checkForSurroundedAntAt w p
        f d x = checkForSurroundedAntAt x $ adjacentCell p d
    in  foldr f w' [East .. NorthEast]

randomInt :: World -> Int -> (Int, World)
randomInt w n = let r  = head $ random w
                    w' = w { random = tail (random w) }
                in (r `mod` n, w')

setCell :: World -> Pos -> Cell -> World
setCell w p c = let c' = M.insert p c (cells w)
                in  w { cells = c' }

setRedHillAt :: World -> Pos -> RedHill
setRedHillAt w p = M.insert p True (redHill w)

setBlackHillAt :: World -> Pos -> BlackHill
setBlackHillAt w p = M.insert p True (blackHill w)


parse :: World -> String -> World
parse w s = result
  where
    (_,_,result) = foldr f (0,0,w) ps
    worldLines   = words s
    sizeX        = (read $ worldLines !! 0) :: Int
    sizeY        = (read $ worldLines !! 1) :: Int
    cellChars    = concat $ drop 2 worldLines
    ps           = zip [ Pos x y | y <- [0 .. sizeY-1], x <- [0 .. sizeX-1]]
                       cellChars
    f :: (Pos, Char) -> (Int, Int, World) -> (Int, Int, World) -- (redAnt id, blackAnt id, world)
    f (p, '#') (r,b,w) = (r,b,setCell w p Rocky)
    f (p, '.') (r,b,w) = (r,b,setCell w p emptyClearCell)
    f (p, '+') (r,b,w) = let a  = mkAnt r Red
                             w' = setAntAt w p a
                             rh = setRedHillAt w p
                         in  (r+1,b,w' { redHill = rh })
    f (p, '-') (r,b,w) = let a  = mkAnt b Black
                             w' = setAntAt w p a
                             bh = setBlackHillAt w p
                         in  (r,b+1,w' { blackHill = bh })
    f (p, n)   (r,b,w)
        | '0' <= n && n <= '9' = (r,b,setFoodAt w p (C.digitToInt n))
