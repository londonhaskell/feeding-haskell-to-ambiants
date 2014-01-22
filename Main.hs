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
    let rs = readBrainState $ lines rb
        bs = readBrainState $ lines bb
        w    = mkWorld rs bs 12345
    putStrLn $ render $ parse w s
