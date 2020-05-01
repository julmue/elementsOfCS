module LogicSeq ( 
    dff
  , bit
  , Signal
  ) where

import Logic
import Arithmetic

import Prelude hiding (iterate, take)

import Data.Stream (Stream(..), (<:>))
import qualified Data.Stream as S
import Control.Monad.State
import Control.Monad

-- Signal = Stream Bit?
type Signal = Stream

-- dff' :: Bit -> State Bit Bit
-- dff' in_ = do
--   past <- get
--   put in_
--   return past

-- test $ evalState (dff clock) O
-- Bit -> Stream Bit -> Stream Bit
dff :: Signal Bit -> State Bit (Signal Bit)
dff signal = do
  past <- get 
  put (S.head signal)
  signal' <- dff (S.tail signal)
  return $ past <:> signal'

bit :: Signal Bit -> Signal Bit -> State Bit (Signal Bit)
bit in_ load = mfix (\out -> dff $ S.zipWith3 mux out in_ load)


-- helpers

i :: Signal Bit
i = S.repeat I 

o :: Signal Bit
o = S.repeat O

clock :: Signal Bit
clock = S.interleave i o

test :: Signal Bit -> [Bit]
test signal = S.take 10 signal

run :: s -> State s a -> a
run = flip evalState
