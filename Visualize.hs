module Visualize
    (
      render
    ) where

import qualified Data.Map as M
import           Geography
import           Geometry
import           Biology

render :: World -> String
render w = (show sizeX ++ "\n" ++ show sizeY) ++ (concat $ map go cs)
  where
    sizeX = (1+) $ maximum $ map (xCoord . fst) cs
    sizeY = (1+) $ maximum $ map (yCoord . fst) cs
    xCoord (Pos x y) = x
    yCoord (Pos x y) = y
    cs    = M.toList $ cells w
    red   = redHill w
    black = blackHill w
    go (Pos x 0, Rocky) | odd x       = "\n # "
                        | otherwise   = "\n# "
    go (_, Rocky)                     = "# "
    go (p, Clear (Just a) (Particles i) _ _)      = if hasFood a
                                                    then show (1 + i) ++ " "
                                                    else if i > 0
                                                         then show i ++ " "
                                                         else case color a of
                                                                Red   -> "R "
                                                                Black -> "B "
    go (p, Clear _ (Particles 0) _ _) = other p red black
    go (_, Clear _ (Particles i) _ _) = show i ++ " "

type Hill = M.Map Pos Bool

other :: Pos -> Hill -> Hill -> String
other p r b = case M.lookup p r of
                Just True -> "+ "
                Nothing   -> case M.lookup p b of
                               Just True -> "- "
                               Nothing   -> ". "
