module NeuroCartography
    (
      readBrainState
    ) where

import qualified Text.ParserCombinators.Parsec as P hiding (spaces)

import           Biology
import           Chemistry
import           Geometry
import qualified Geometry      as G
import           Neurology
import           Phenomenology

parseSense :: P.Parser Instruction
parseSense = do
    token <- P.many P.letter
    return $ case token of
               "Sense"  -> Sense Here (mkState 0) (mkState 0) Friend
               "Mark"   -> Mark (mkMarker 0) (mkState 0)
               "Unmark" -> Unmark (mkMarker 0) (mkState 0)
               "PickUp" -> PickUp (mkState 0) (mkState 0)
               "Drop"   -> Drop (mkState 0)
               "Turn"   -> Turn G.Left (mkState 0)
               "Move"   -> Move (mkState 0) (mkState 0)
               "Flip"   -> Flip 2 (mkState 0) (mkState 0)

readBrainState s = case P.parse parseSense "Brain State" s of
                     Prelude.Left  err -> "ERROR parsing: " ++ show err
                     Prelude.Right val -> show val
