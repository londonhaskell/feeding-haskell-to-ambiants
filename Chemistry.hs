module Chemistry
    (
      Marker
    , emptyMarker
    , setMarker
    , clearMarker
    , checkMarker
    , checkAnyMarker
    ) where

import qualified Data.Map as M

data Marker = Marker (M.Map Int Bool)
            deriving (Show, Eq, Read)

emptyMarker :: Marker
emptyMarker = Marker M.empty

setMarker :: Marker -> Int -> Marker
setMarker (Marker m) i
  | inRange i = Marker $ M.insert i True m
  | otherwise = error("Incorrect marker value: " ++ show i)

clearMarker :: Marker -> Int -> Marker
clearMarker (Marker m) i
  | inRange i = Marker $ M.delete i m
  | otherwise = error("Incorrect marker value: " ++ show i)

checkMarker :: Marker -> Int -> Bool
checkMarker (Marker m) i
  | inRange i = M.member i m
  | otherwise = error("Incorrect marker value: " ++ show i)

checkAnyMarker :: Marker -> Bool
checkAnyMarker (Marker m) = M.null m

inRange :: Int -> Bool
inRange i = i >= 0 && i <= 5
