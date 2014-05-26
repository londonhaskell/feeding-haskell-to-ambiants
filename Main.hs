module Main where

import Data.Map (keys)
import Data.List (unfoldr)
import System.Environment

import Ants
import Visualize
import Geography
import NeuroCartography
import Kinetics

main :: IO ()
main = do
    args <- getArgs
    s  <- readFile $ args !! 0  -- world file
    rb <- readFile $ args !! 1  -- red brain
    bb <- readFile $ args !! 2  -- black brain
    let i  = read (args !! 3) :: Int
        rs = readBrainState $ lines rb
        bs = readBrainState $ lines bb
        w  = parse (mkWorld rs bs 12345) s
    mapM_ printInfo $ take i $ go w

printInfo w = do
    putStrLn $ render w
    putStrLn $ show . length . keys $ ants w

go w = unfoldr (\x -> Just (x, multistep x)) w
