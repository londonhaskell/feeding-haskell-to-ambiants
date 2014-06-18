module Kinetics
    (
      multistep
    ) where

import Data.List
import Data.Map (keys)

import Biology
import Chemistry
import Geography
import Geometry
import Neurology

multistep :: World -> World
multistep w = let is = keys $ ants w
              in  foldl' step w is

step :: World -> Int -> World
step w id =
    if not $ antIsAlive w id
    then w
    else let p = findAnt w id
             a = antAt w p
         in  if   resting a > 0
             then setAntAt w p (a { resting = (resting a) - 1 })
             else go (getInstruction w (color a) (state a)) w p a id

go :: Instruction -> World -> Pos -> Ant -> Int -> World
go (Sense senseDir st1 st2 cond) w p a id = let p' = sensedCell p (currentDir a) senseDir
                                                st = if   cellMatches w p' cond (color a)
                                                     then st1
                                                     else st2
                                            in  setAntAt w p (a { state = st })

go (Mark m s)                    w p a id = let w' = setMarkerAt w p (color a) m
                                            in  setAntAt w' p (a { state = s })
go (Unmark m s)                  w p a id = let w' = clearMarkerAt w p (color a) m
                                            in  setAntAt w' p (a { state = s })

go (PickUp st1 st2)              w p a id = if   hasFood a || foodAt w p == 0
                                            then setAntAt w p (a { state = st2 })
                                            else let w' = setFoodAt w p $ foodAt w p - 1
                                                 in  setAntAt w' p (a { state   = st1
                                                                      , hasFood = True
                                                                      })

go (Drop st)                     w p a id = let a'   = a { hasFood = False }
                                                w'   = setFoodAt w p $ foodAt w p + 1
                                                w''  = setAntAt w' p a'
                                                w''' = if   hasFood a
                                                       then setAntAt w'' p (a' { state = st })
                                                       else setAntAt w   p (a  { state = st })
                                            in  w'''

go (Turn lr st)                  w p a id = setAntAt w p (a { currentDir = (turn lr $ currentDir a)
                                                            , state      = st
                                                            })

go (Move st1 st2)                w p a id = let p' = adjacentCell p (currentDir a)
                                            in  if   rocky w p' || someAntIsAt w p'
                                                then setAntAt w p (a { state = st2 })
                                                else let w'  = clearAntAt w p
                                                         w'' = setAntAt w' p' (a { state = st1
                                                                                 , resting = 14
                                                                                 })
                                                     in  checkForSurroundedAnts w'' p'
go (Flip n st1 st2)              w p a id = let (r,w') = randomInt w n
                                                st     = if   r == 0
                                                         then st1
                                                         else st2
                                            in setAntAt w' p (a { state = st })
