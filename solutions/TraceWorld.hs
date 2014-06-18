module TraceWorld
where

import Prelude hiding ( id, lookup )
import World
import Engine
import Data.List ( intercalate, (\\) )
import Data.Char ( toLower, chr )
import Data.Map ( Map, (!), fromList, toList, member,
                empty, keys, elems, lookup )

traceWorld :: World -> [String]
traceWorld w = [ tracePos p ++ traceCell h c
                        | y <- [0 .. (worldSizeY w) - 1 ]
                        , x <- [0 .. (worldSizeX w) - 1 ]
                        , let p = (x,y)
                        , let c = (cells w) ! p
                        , let h = lookup p (hills w)
                        ]

-- Example: "cell (4, 8): "
tracePos :: Pos -> String
tracePos (x,y) = "cell (" ++ show x ++ ", " ++ show y ++ "): "

traceCell :: Maybe Color -> Cell -> String
traceCell h  Rocky        = "rock"
traceCell h (ClearCell c) = traceCellContents h c
traceCell h (AntCell a c) = traceCellContents h c ++ traceAnt a

traceCellContents :: Maybe Color -> CellContents -> String
traceCellContents h c = traceFood c ++ traceHill h ++ 
                                        traceMarkers Red (markers c) ++ 
                                        traceMarkers Black (markers c)

-- Example: "black ant of id 0, dir 0, food 0, state 0, resting 0"
traceAnt :: Ant -> String
traceAnt a = intercalate ", "
                [ traceColor (color a) ++ " ant of id " ++ show (id a)
                , "dir " ++ traceDir (direction a)
                , "food " ++ (if has_food a then "1" else "0")
                , "state " ++ show (state a)
                , "resting " ++ show (resting a)
                ]

-- Example: "9 food; "
traceFood :: CellContents -> String
traceFood c = if f > 0 then show f ++ " food; " else ""
    where f = food c

-- Example: "red hill; "
traceHill :: Maybe Color -> String
traceHill Nothing              = ""
traceHill (Just c) | c == Red  = "red hill; "
                   | otherwise = "black hill; "

-- Example: "red marks: 0124; black marks: 2; "
traceMarkers :: Color -> Markers -> String
traceMarkers col ms = if null marks then ""
                                   else traceColor col ++ " marks: " ++ marks ++ "; "
    where marks = concat [ s | (c, m) <- keys ms
                                    , c == col -- Filter to the colour passed 
                                    , ms ! (c, m) -- Check if the mark is set and use as a filter
                                    , let s = drop 1 (show m) -- Get rid of the M at the start
                                    ] 

traceColor :: Color -> String
traceColor = (map toLower) . show

traceDir :: Dir -> String
traceDir East      = "0"
traceDir SouthEast = "1"
traceDir SouthWest = "2"
traceDir West      = "3"
traceDir NorthWest = "4"
traceDir NorthEast = "5"

-- validateWorld takes an error message and a world and checks that 
-- the position map is correct. Acts as identity on world so can be 
-- used in-line anywhere a world is used as assertion code
validateWorld :: String -> World -> World
validateWorld msg w = if valid
                      then w
                      else error ( "validateWorld:" ++ msg ++ "\n" ++ differences )
    where valid = validPosition
          validPosition = calcPositions == (antPositions w)
          calcPositions = fromList [ (id a, p) | (p, AntCell a _) <- toList (cells w) ] 
          differences = show ((toList (antPositions w)) \\ (toList calcPositions)) ++ "\n" ++
                         show ((toList calcPositions) \\ (toList (antPositions w)))
          calcPos :: World -> Map Integer Pos
          calcPos w = fromList [ (id a, p) | (p, AntCell a _) <- toList (cells w) ]
