module Main where

import Ants
import Visualize
import Geography

main :: IO ()
main = do
    s <- readFile "world.txt"
    print $ render $ parse (mkWorld 12345) s
