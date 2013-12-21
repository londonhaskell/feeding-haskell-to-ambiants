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
parseInstruction = try (parseMark)     -- Move starts with 'M' so *try* Mark
               <|> parseUnmark
               <|> parsePickUp
               <|> parseDrop
               <|> parseTurn
               <|> parseMove
               <|> parseFlip

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

parseDrop :: Parser Instruction
parseDrop = do
    string "Drop"
    char ' '
    st <- many digit
    return $ Drop (mkState (read st :: Int))

parseTurn :: Parser Instruction
parseTurn = do
    string "Turn"
    char ' '
    turn <- string "Left" <|> string "Right"
    char ' '
    st <- many digit
    return $ Turn (if turn !! 0 == 'L' then G.Left else G.Right)
                  (mkState (read st :: Int))

parseMove :: Parser Instruction
parseMove = do
    string "Move"
    char ' '
    st1 <- many digit
    char ' '
    st2 <- many digit
    return $ Move (mkState (read st1 :: Int))
                  (mkState (read st2 :: Int))

parseFlip :: Parser Instruction
parseFlip = do
    string "Flip"
    char ' '
    n <- many digit
    char ' '
    st1 <- many digit
    char ' '
    st2 <- many digit
    return $ Flip (read n :: Int)
                  (mkState (read st1 :: Int))
                  (mkState (read st2 :: Int))

readBrainState s = case parse parseInstruction "Brain State" s of
                     Prelude.Left  err -> "ERROR parsing: " ++ show err
                     Prelude.Right val -> show val
