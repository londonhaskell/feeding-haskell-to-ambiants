module Neurology
    (
      Instruction(..)
    ) where

import Biology
import Chemistry
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
