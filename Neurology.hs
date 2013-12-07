module Neurology
    (
      Instruction(..)
    , getInstruction
    ) where

import Biology
import Chemistry
import Geography
import Geometry
import Phenomenology

data Instruction = Sense SenseDir State State Condition
                 | Mark Marker State
                 | Unmark Marker State
                 | PickUp State State
                 | Drop State
                 | Turn Turn State
                 | Move State State
                 | Flip Int State State
                 deriving (Show, Eq, Read)

-- Keep the ant state machine in the world?
getInstruction :: World -> Color -> State -> Instruction
getInstruction w c s = undefined
