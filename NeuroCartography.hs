module NeuroCartography
    (
      readBrainState
    ) where

import           Text.ParserCombinators.Parsec

import           Biology
import           Chemistry
import           Geometry
import qualified Geometry as G
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
    marker <- parseInt
    char ' '
    st <- parseInt
    return $ Mark (mkMarker marker) (mkState st)

parseUnmark :: Parser Instruction
parseUnmark = do
    string "Unmark"
    char ' '
    marker <- parseInt
    char ' '
    st <- parseInt
    return $ Unmark (mkMarker marker) (mkState st)

parsePickUp :: Parser Instruction
parsePickUp = do
    string "PickUp"
    char ' '
    st1 <- parseInt
    char ' '
    st2 <- parseInt
    return $ PickUp (mkState st1) (mkState st2)

parseDrop :: Parser Instruction
parseDrop = do
    string "Drop"
    char ' '
    st <- parseInt
    return $ Drop (mkState st)

parseTurn :: Parser Instruction
parseTurn = do
    string "Turn"
    char ' '
    turn <- string "Left" <|> string "Right"
    char ' '
    st <- parseInt
    return $ Turn (if turn !! 0 == 'L' then G.Left else G.Right) (mkState st)

parseMove :: Parser Instruction
parseMove = do
    string "Move"
    char ' '
    st1 <- parseInt
    char ' '
    st2 <- parseInt
    return $ Move (mkState st1) (mkState st2)

parseFlip :: Parser Instruction
parseFlip = do
    string "Flip"
    char ' '
    n <- parseInt
    char ' '
    st1 <- parseInt
    char ' '
    st2 <- parseInt
    return $ Flip n (mkState st1) (mkState st2)

parseInt :: Parser Int
parseInt = do
    n <- many digit
    return (read n :: Int)


readBrainState s = case parse parseInstruction "Brain State" s of
                     Prelude.Left  err -> "ERROR parsing: " ++ show err
                     Prelude.Right val -> show val
