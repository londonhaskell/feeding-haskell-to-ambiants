module Visualize
    (
      render
    ) where

import qualified Data.Map as M
import           Geography
import           Geometry

render :: World -> [(Int, Int, String)]
render w = map go cs
  where
    cs    = M.toList $ cells w
    red   = redHill w
    black = blackHill w
    go (Pos x y,     Rocky)                     = (x, y, "# ")
    go (p@(Pos x y), Clear _ (Particles 0) _ _) = (x, y, other p red black)
    go (Pos x y,     Clear _ (Particles i) _ _) = (x, y, show i ++ " ")

type Hill = M.Map Pos Bool

other :: Pos -> Hill -> Hill -> String
other p r b = case M.lookup p r of
                Just True -> "+ "
                Nothing   -> case M.lookup p b of
                               Just True -> "- "
                               Nothing   -> ". "
