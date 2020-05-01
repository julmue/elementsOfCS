module LogicSeq where

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

i :: Signal Bit
i = S.repeat I 

o :: Signal Bit
o = S.repeat O

clock :: Signal Bit
clock = S.interleave i o

test :: Signal Bit -> [Bit]
test signal = S.take 10 signal

register :: Signal Bit -> Signal Bit -> State Bit (Signal Bit)
register in_ load = mfix (\out -> dff $ S.zipWith3 mux in_ out load)


-- https://hackage.haskell.org/package/Stream-0.4.7.2/docs/src/Data-Stream.html
-- stream
-- think of parser combinators ...
-- infixr 5 :> 
-- data Stream a = a :> Stream a deriving (Show)

-- iterate :: (a -> a) -> a -> Stream a
-- iterate f x = x :> (iterate f (f x))

-- take :: Int -> Stream a  -> [a]
-- take n ~(x :> xs)
--   | n == 0    = []
--   | n > 0     =  x : (take (n - 1) xs)
--   | otherwise = error "Stream.take: negative argument."
-- count = 1 :> count + 1