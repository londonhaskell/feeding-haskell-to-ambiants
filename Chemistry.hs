module Chemistry
    (
      Marker
    , mkMarker
    , marker
    ) where

import qualified Data.Map as M

data Marker = Marker Int
            deriving (Show, Eq, Read, Ord)

mkMarker :: Int -> Marker
mkMarker i
  | inRange i = Marker i
  | otherwise = error("Incorrect marker value: " ++ show i)

inRange :: Int -> Bool
inRange i = i >= 0 && i <= 5

marker :: Marker -> Int
marker (Marker i) = i
