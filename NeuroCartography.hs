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
               <|> parseSense

parseMark :: Parser Instruction
parseMark = do
    string "Mark"
    char ' '
    marker <- parseMarker
    char ' '
    st <- parseState
    return $ Mark marker st

parseUnmark :: Parser Instruction
parseUnmark = do
    string "Unmark"
    char ' '
    marker <- parseMarker
    char ' '
    st <- parseState
    return $ Unmark marker st

parsePickUp :: Parser Instruction
parsePickUp = do
    string "PickUp"
    char ' '
    st1 <- parseState
    char ' '
    st2 <- parseState
    return $ PickUp st1 st2

parseDrop :: Parser Instruction
parseDrop = do
    string "Drop"
    char ' '
    st <- parseState
    return $ Drop st

parseTurn :: Parser Instruction
parseTurn = do
    string "Turn"
    char ' '
    turn <- string "Left" <|> string "Right"
    char ' '
    st <- parseState
    return $ Turn (if turn !! 0 == 'L' then G.Left else G.Right) st

parseMove :: Parser Instruction
parseMove = do
    string "Move"
    char ' '
    st1 <- parseState
    char ' '
    st2 <- parseState
    return $ Move st1 st2

parseFlip :: Parser Instruction
parseFlip = do
    string "Flip"
    char ' '
    n <- parseInt
    char ' '
    st1 <- parseState
    char ' '
    st2 <- parseState
    return $ Flip n st1 st2

parseInt :: Parser Int
parseInt = do
    n <- many digit
    return (read n :: Int)

parseSenseDir :: Parser SenseDir
parseSenseDir = do
    dir <-     string "Here"
           <|> string "Ahead"
           <|> string "LeftAhead"
           <|> string "RightAhead"
    return (read dir :: SenseDir)

parseCondition :: Parser Condition
parseCondition = do
    cond <-     try (string "FriendWithFood")
            <|> try (string "Friend")
            <|> try (string "FoeWithFood")
            <|> try (string "FoeMarker")
            <|> try (string "FoeHome")
            <|> try (string "Foe")
            <|> string "Food"
            <|> string "Rock"
            <|> string "Home"
            <|> string "Marker"
    case cond of
      "Marker" -> do
          char ' '
          i <- parseMarker
          return $ Marker i
      x        -> return (read x :: Condition)

parseSense :: Parser Instruction
parseSense = do
    string "Sense"
    char ' '
    dir <- parseSenseDir
    char ' '
    st1 <- parseState
    char ' '
    st2 <- parseState
    char ' '
    cond <- parseCondition
    return $ Sense dir st1 st2 cond

parseState :: Parser Biology.State
parseState = do
    i <- parseInt
    return $ mkState i

parseMarker :: Parser Marker
parseMarker = do
    i <- parseInt
    return $ mkMarker i

readBrainState s = case parse parseInstruction "Brain State" s of
                     Prelude.Left  err -> "ERROR parsing: " ++ show err
                     Prelude.Right val -> show val
