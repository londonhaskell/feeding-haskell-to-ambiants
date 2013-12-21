module NeuroCartography
    (
      readBrainState
    ) where

import qualified Data.Char                     as C        (digitToInt)
import qualified Text.ParserCombinators.Parsec as P hiding (spaces)

import           Biology
import           Chemistry
import           Geometry
import qualified Geometry      as G
import           Neurology
import           Phenomenology

parseInstruction :: P.Parser Instruction
parseInstruction = parseMark

parseMark :: P.Parser Instruction
parseMark = do
    token <- P.many P.letter
    P.char ' '
    marker <- P.digit
    P.char ' '
    st <- P.many P.digit
    return $ case token of
               "Mark" -> Mark (mkMarker $ C.digitToInt marker)
                              (mkState (read st :: Int))

readBrainState s = case P.parse parseInstruction "Brain State" s of
                     Prelude.Left  err -> "ERROR parsing: " ++ show err
                     Prelude.Right val -> show val
