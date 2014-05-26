module Main where

import System.Environment

import Ants
import Visualize
import Geography
import NeuroCartography

main :: IO ()
main = do
    args <- getArgs
    s  <- readFile $ args !! 0  -- world file
    rb <- readFile $ args !! 1  -- red brain
    bb <- readFile $ args !! 2  -- black brain
    let i  = read (args !! 3) :: Int
        rs = readBrainState $ lines rb
        bs = readBrainState $ lines bb
        w    = mkWorld rs bs 12345
    putStrLn $ render $ parse w s
