module Main
where

import World
import Engine
import BrainParser
import TraceWorld
import System.IO
import Data.List ( foldl' )
import Data.Map ( keys )
import Control.Monad ( when )

main :: IO ()
main = mapM_ runTrace $ zip [ 0, 1000 .. 9000 ] [ 1000, 2000 .. 10000 ]

runTrace :: (Integer, Integer) -> IO ()
runTrace (lowerLimit, upperLimit) = do
    putStrLn ("Trace from " ++ show lowerLimit ++ " to " ++ show upperLimit)
    let seed = 12345
    g <- createGame seed
    let dumpFilePath = ("test/dump." ++ show lowerLimit ++ "-" ++ show upperLimit)
    h <- openFile dumpFilePath WriteMode
    hSetNewlineMode h noNewlineTranslation
    hPutStr h ("random seed: " ++ show seed ++ "\n")
    runGame h lowerLimit upperLimit 10000 g
    hClose h
    return ()

createGame :: Integer -> IO Game
createGame seed = do
    s  <- readFile "test/sample-world.txt"
    i <- readFile "test/sample.txt"
    let b = parseBrain $ lines i
        w  = parseWorld s
        g = mkGame seed [ (Red, b), (Black, b) ] w
    return g

runGame :: Handle -> Integer -> Integer -> Integer -> Game -> IO Game
runGame h lowerLimit upperLimit gameLimit g = do
    let printStep = gameLimit `quot` 10
    when ( gameRound g `mod` printStep == 0 )
            ( putStrLn ( "Round " ++ show (gameRound g) ) )
    when ( gameRound g >= lowerLimit && gameRound g <= upperLimit )
            ( do { hPutStr h "\nAfter round " ;
                   hPutStr h (show $ gameRound g) ;
                   hPutStr h "...\n" ;
                   hPutStr h (unlines $ traceWorld (world g)) } )
    if (gameRound g) >= gameLimit
      then return g
      else runGame h lowerLimit upperLimit gameLimit g''
        where g' = multiStep g
              g'' = g' { gameRound = (gameRound g') + 1 } 

multiStep :: Game -> Game
multiStep g = let ids = keys . antPositions . world $ g
              in  foldl' step g ids
