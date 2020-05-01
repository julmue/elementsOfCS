module LogicSeq ( 
    dff
  , bit
  , register
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

register :: Signal Bit16 -> Signal Bit -> State Bit16 (Signal Bit16)
register in_ load = do 
  (s01,s02,s03,s04,s05,s06,s07,s08,s09,s10,s11,s12,s13,s14,s15,s16) <- get
  let (in_01,in_02,in_03,in_04,in_05,in_06,in_07,in_08,in_09,in_10,in_11,in_12,in_13,in_14,in_15,in_16) = unzipSignalBit16 in_
      (out01, s01') = runBit in_01 s01
      (out02, s02') = runBit in_02 s02 
      (out03, s03') = runBit in_03 s03 
      (out04, s04') = runBit in_04 s04 
      (out05, s05') = runBit in_05 s05 
      (out06, s06') = runBit in_06 s06 
      (out07, s07') = runBit in_07 s07 
      (out08, s08') = runBit in_08 s08 
      (out09, s09') = runBit in_09 s09 
      (out10, s10') = runBit in_10 s10 
      (out11, s11') = runBit in_11 s11 
      (out12, s12') = runBit in_12 s12
      (out13, s13') = runBit in_13 s13
      (out14, s14') = runBit in_14 s14
      (out15, s15') = runBit in_15 s15
      (out16, s16') = runBit in_16 s16 
  put (s01',s02',s03',s04',s05',s06',s07',s08',s09',s10',s11',s12',s13',s14',s15',s16')
  return $ zipSignalBit16 (out01,out02,out03,out04,out05,out06,out07,out08,out09,out10,out11,out12,out13,out14,out15,out16)
    where 
      runBit :: Signal Bit -> Bit -> (Signal Bit, Bit)
      runBit in_ initial = runState (bit in_ load) initial

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




-- helpers

i :: Signal Bit
i = S.repeat I 

i16 :: Signal Bit16
i16 = fmap replicateBit16 i

o :: Signal Bit
o = S.repeat O

_O16 :: Bit16
_O16 = stringToBit16 "OOOOOOOOOOOOOOOO"

_I16 :: Bit16
_I16 = stringToBit16 "IIIIIIIIIIIIIIII"

o16 :: Signal Bit16
o16 = fmap replicateBit16 o

clock :: Signal Bit
clock = S.interleave i o

test :: Signal Bit -> [Bit]
test signal = S.take 10 signal

run :: s -> State s a -> a
run = flip evalState
