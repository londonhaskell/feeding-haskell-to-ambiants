module Main where

import Ants
import Visualize
import Geography
import NeuroCartography

main :: IO ()
main = do
    s  <- readFile "world2.txt"
    rb <- readFile "redBrain.txt"
    bb <- readFile "blackBrain.txt"
    let rIns = readBrainState $ lines rb
        bIns = readBrainState $ lines bb
    print $ render $ parse (mkWorld rIns bIns 12345) s
