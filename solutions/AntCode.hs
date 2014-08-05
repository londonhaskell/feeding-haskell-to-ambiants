-- {-# LANGUAGE GADTs #-} 
 
module AntCode where

import World
-- import Engine

-- Based on "Why free monads matter" blog post
-- http://www.haskellforall.com/2012/06/you-could-have-invented-free-monads.html

data Free f r = Free (f (Free f r))
              | Pure r
-- r is the final output type
-- f is the recursive step and we are creating the 
-- type by finding the fixed point of f

instance Functor f => Monad (Free f) where
    return         = Pure
    (Free x) >>= f = Free (fmap (>>= f) x)
    (Pure r) >>= f = f r

liftF :: (Functor f) => f r -> Free f r
liftF c = Free (fmap Pure c)

----------------------------------------------------------------
    
data AntCommand next = CmdSense Condition Dir AntProgram next
                     | CmdMark Marker next
                     | CmdUnmark Marker next
                     | CmdPickUp next AntProgram
                     | CmdDrop next
                     | CmdTurn Left_Or_Right next
                     | CmdMove next AntProgram 
                     | CmdFlip Integer AntProgram next
                     | Label JumpLabel next
                     | Jump JumpLabel

-- Where there are two follow on states we need to treat them differently
-- only one is the free one which is the following line

data JumpLabel = JumpLabel String
               | ProgramCounter Integer

type AntProgram = Free AntCommand ()

-- Need fmap :: (a -> b) -> m a -> m b
-- to fake the Monad types with
-- bind :: a -> (a -> m b) -> m b
-- even though the types aren't actually changing
instance Functor (AntCommand) where
    fmap f (CmdSense c d branch n) = CmdSense c d branch (f n)
    fmap f (CmdMark m n)           = CmdMark m (f n)
    fmap f (CmdUnmark m n)         = CmdUnmark m (f n)
    fmap f (CmdPickUp n1 err)      = CmdPickUp (f n1) err
    fmap f (CmdDrop n)             = CmdDrop (f n)
    fmap f (CmdTurn lr n)          = CmdTurn lr (f n)
    fmap f (CmdMove n1 err)        = CmdMove (f n1) err
    fmap f (CmdFlip i branch n2)   = CmdFlip i branch (f n2)
    fmap f (Label t n)             = Label t (f n)
    fmap f (Jump j)                = Jump j

----------------------------------------------------------------

-- Primitives commands
    
senseIf :: Condition -> Dir -> AntProgram -> AntProgram
senseIf c d p = liftF $ CmdSense c d p ()
    
mark :: Integer -> AntProgram
mark n = liftF $ CmdMark (mkMarker n) ()

unmark :: Integer -> AntProgram
unmark n = liftF $ CmdUnmark (mkMarker n) ()

pickUpOr :: AntProgram -> AntProgram
pickUpOr err = liftF $ CmdPickUp () err

drop :: AntProgram
drop = liftF $ CmdDrop ()

turn :: Left_Or_Right -> AntProgram
turn lr = liftF $ CmdTurn lr ()

moveOr :: AntProgram -> AntProgram
moveOr err = liftF $ CmdMove () err

oneIn :: Integer -> AntProgram -> AntProgram
oneIn i p | i == 1    = p 
          | otherwise = liftF $ CmdFlip i p ()

label :: String -> AntProgram
label s = liftF $ Label (JumpLabel s) ()

jump :: String -> AntProgram
jump s = liftF $ Jump (JumpLabel s)

----------------------------------------------------------------

-- Compound commands

pick :: [AntProgram] -> AntProgram
pick [] = error "pick []"
pick [x] = x
pick (x:xs) = do
    let l = fromIntegral $ length (x:xs)
    oneIn l x
    pick xs
