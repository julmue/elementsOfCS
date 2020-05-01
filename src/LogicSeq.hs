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

register :: Signal Bit16 -> Signal Bit -> State Bit (Signal Bit16)
register = undefined

mux16' :: (a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a) -> Bit -> Bit -> Bit -> Bit -> a
mux16' ~(b01,b02,b03,b04,b05,b06,b07,b08,b09,b10,b11,b12,b13,b14,b15,b16) sel1 sel2 sel3 sel4 =
  case (sel1, sel2, sel3, sel4) of
    (O,O,O,O) -> b01
    (O,O,O,I) -> b02
    (O,O,I,O) -> b03
    (O,O,I,I) -> b04
    (O,I,O,O) -> b05
    (O,I,O,I) -> b06
    (O,I,I,O) -> b07
    (O,I,I,I) -> b08
    (I,O,O,O) -> b09
    (I,O,O,I) -> b10
    (I,O,I,O) -> b11
    (I,O,I,I) -> b12
    (I,I,O,O) -> b13
    (I,I,O,I) -> b14
    (I,I,I,O) -> b15
    (I,I,I,I) -> b16

_01 b16 = mux16' b16 O O O O
_02 b16 = mux16' b16 O O O I
_03 b16 = mux16' b16 O O I O
_04 b16 = mux16' b16 O O I I
_05 b16 = mux16' b16 O I O O
_06 b16 = mux16' b16 O I O I
_07 b16 = mux16' b16 O I I O
_08 b16 = mux16' b16 O I I I
_09 b16 = mux16' b16 I O O O
_10 b16 = mux16' b16 I O O I
_11 b16 = mux16' b16 I O I O
_12 b16 = mux16' b16 I O I I
_13 b16 = mux16' b16 I I O O
_14 b16 = mux16' b16 I I O I
_15 b16 = mux16' b16 I I I O
_16 b16 = mux16' b16 I I I I

unzipSignalBit16 :: 
  Signal Bit16 -> 
  (Signal Bit, Signal Bit, Signal Bit, Signal Bit, Signal Bit, Signal Bit, Signal Bit, Signal Bit, 
   Signal Bit, Signal Bit, Signal Bit, Signal Bit, Signal Bit, Signal Bit, Signal Bit, Signal Bit)
unzipSignalBit16 ~(s `Cons` sig) =
  ( Cons (_01 s) (_01 (unzipSignalBit16 sig))
  , Cons (_02 s) (_02 (unzipSignalBit16 sig))
  , Cons (_03 s) (_03 (unzipSignalBit16 sig))
  , Cons (_04 s) (_04 (unzipSignalBit16 sig))
  , Cons (_05 s) (_05 (unzipSignalBit16 sig))
  , Cons (_06 s) (_06 (unzipSignalBit16 sig))
  , Cons (_07 s) (_07 (unzipSignalBit16 sig))
  , Cons (_08 s) (_08 (unzipSignalBit16 sig))
  , Cons (_09 s) (_09 (unzipSignalBit16 sig))
  , Cons (_10 s) (_10 (unzipSignalBit16 sig))
  , Cons (_11 s) (_11 (unzipSignalBit16 sig))
  , Cons (_12 s) (_12 (unzipSignalBit16 sig))
  , Cons (_13 s) (_13 (unzipSignalBit16 sig))
  , Cons (_14 s) (_14 (unzipSignalBit16 sig))
  , Cons (_15 s) (_15 (unzipSignalBit16 sig))
  , Cons (_16 s) (_16 (unzipSignalBit16 sig))
  )

zipSignalBit16 :: 
  (Signal Bit, Signal Bit, Signal Bit, Signal Bit, Signal Bit, Signal Bit, Signal Bit, Signal Bit, 
   Signal Bit, Signal Bit, Signal Bit, Signal Bit, Signal Bit, Signal Bit, Signal Bit, Signal Bit)
  -> Signal Bit16
zipSignalBit16 
  ( s01 `Cons` sig01 
  , s02 `Cons` sig02
  , s03 `Cons` sig03
  , s04 `Cons` sig04
  , s05 `Cons` sig05
  , s06 `Cons` sig06
  , s07 `Cons` sig07
  , s08 `Cons` sig08
  , s09 `Cons` sig09
  , s10 `Cons` sig10
  , s11 `Cons` sig11
  , s12 `Cons` sig12
  , s13 `Cons` sig13
  , s14 `Cons` sig14
  , s15 `Cons` sig15
  , s16 `Cons` sig16 ) = 
  (s01,s02,s03,s04,s05,s06,s07,s08,s09,s10,s11,s12,s13,s14,s15,s16) `Cons` 
  (zipSignalBit16 (sig01,sig02,sig03,sig04,sig05,sig06,sig07,sig08,sig09,sig10,sig11,sig12,sig13,sig14,sig15,sig16))


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
