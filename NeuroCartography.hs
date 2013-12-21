module NeuroCartography
    (
      readBrainState
    ) where

import qualified Data.Char                     as C        (digitToInt)
import           Text.ParserCombinators.Parsec

import           Biology
import           Chemistry
import           Geometry
import qualified Geometry      as G
import           Neurology
import           Phenomenology

parseInstruction :: Parser Instruction
parseInstruction = parseMark
               <|> parseUnmark
               <|> parsePickUp

parseMark :: Parser Instruction
parseMark = do
    string "Mark"
    char ' '
    marker <- digit
    char ' '
    st <- many digit
    return $ Mark (mkMarker $ C.digitToInt marker)
                  (mkState (read st :: Int))

parseUnmark :: Parser Instruction
parseUnmark = do
    string "Unmark"
    char ' '
    marker <- digit
    char ' '
    st <- many digit
    return $ Unmark (mkMarker $ C.digitToInt marker)
                    (mkState (read st :: Int))

parsePickUp :: Parser Instruction
parsePickUp = do
    string "PickUp"
    char ' '
    st1 <- many digit
    char ' '
    st2 <- many digit
    return $ PickUp (mkState (read st1 :: Int))
                    (mkState (read st2 :: Int))

readBrainState s = case parse parseInstruction "Brain State" s of
                     Prelude.Left  err -> "ERROR parsing: " ++ show err
                     Prelude.Right val -> show val
