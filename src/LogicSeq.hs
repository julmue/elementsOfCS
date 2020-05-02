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
      (out01, s01') = runBit in_01 load s01
      (out02, s02') = runBit in_02 load s02 
      (out03, s03') = runBit in_03 load s03 
      (out04, s04') = runBit in_04 load s04 
      (out05, s05') = runBit in_05 load s05 
      (out06, s06') = runBit in_06 load s06 
      (out07, s07') = runBit in_07 load s07 
      (out08, s08') = runBit in_08 load s08 
      (out09, s09') = runBit in_09 load s09 
      (out10, s10') = runBit in_10 load s10 
      (out11, s11') = runBit in_11 load s11 
      (out12, s12') = runBit in_12 load s12
      (out13, s13') = runBit in_13 load s13
      (out14, s14') = runBit in_14 load s14
      (out15, s15') = runBit in_15 load s15
      (out16, s16') = runBit in_16 load s16 
  put (s01',s02',s03',s04',s05',s06',s07',s08',s09',s10',s11',s12',s13',s14',s15',s16')
  return $ zipSignalBit16 (out01,out02,out03,out04,out05,out06,out07,out08,out09,out10,out11,out12,out13,out14,out15,out16)
    where 
      
runBit :: Signal Bit -> Signal Bit -> Bit -> (Signal Bit, Bit)
runBit in_ load initial = runState (bit in_ load) initial

-- inputs: 
--    in[16]
--    address[3]
--    load
-- out:
--    out[16]
-- function:
--    out(t)=RAM[adress(t)](t)
--    
ram8 :: Signal Bit16 -> Signal Bit3 -> Signal Bit -> State (Vector8 Bit16) (Signal Bit16)
ram8 in_ address load = do
    (Vector8 s01 s02 s03 s04 s05 s06 s07 s08) <- get
    let (load01, load02, load03, load04, load05, load06, load07, load08) = unzipSignalBit8 $ S.zipWith dmux8Way load address
        (out01, s01') = runRegister in_ load01 s01
        (out02, s02') = runRegister in_ load02 s02
        (out03, s03') = runRegister in_ load03 s03
        (out04, s04') = runRegister in_ load04 s04
        (out05, s05') = runRegister in_ load05 s05
        (out06, s06') = runRegister in_ load06 s06
        (out07, s07') = runRegister in_ load07 s07
        (out08, s08') = runRegister in_ load08 s08
    put (Vector8 s01' s02' s03' s04' s05' s06' s07' s08')
    return $ zipWith9 mux8Way16 out01 out02 out03 out04 out05 out06 out07 out08 address 

runRegister :: Signal Bit16 -> Signal Bit -> Bit16 -> (Signal Bit16, Bit16) 
runRegister in_ load initial = runState (register in_ load) initial

-- Vector8 Bit16
data Vector8 a = Vector8 {
  _vector81 :: a,
  _vector82 :: a,
  _vector83 :: a,
  _vector84 :: a,
  _vector85 :: a,
  _vector86 :: a,
  _vector87 :: a,
  _vector88 :: a
  } deriving (Show, Read, Eq)

zipWith9 :: (a -> b -> c -> d -> e -> f -> g -> h -> i -> j) 
  -> Stream a
  -> Stream b
  -> Stream c
  -> Stream d
  -> Stream e
  -> Stream f
  -> Stream g
  -> Stream h
  -> Stream i
  -> Stream j
zipWith9 f (h1 `Cons` s1) (h2 `Cons` s2) (h3 `Cons` s3) (h4 `Cons` s4) (h5 `Cons` s5) (h6 `Cons` s6) (h7 `Cons` s7) (h8 `Cons` s8) (h9 `Cons` s9) =
  f h1 h2 h3 h4 h5 h6 h7 h8 h9 `Cons` zipWith9 f s1 s2 s3 s4 s5 s6 s7 s8 s9 

unzipSignalBit8 :: 
  Signal Bit8 -> 
  (Signal Bit, Signal Bit, Signal Bit, Signal Bit, Signal Bit, Signal Bit, Signal Bit, Signal Bit)
unzipSignalBit8 ~(s `Cons` sig) =
  ( Cons (_01_8 s) (_01_8 (unzipSignalBit8 sig))
  , Cons (_02_8 s) (_02_8 (unzipSignalBit8 sig))
  , Cons (_03_8 s) (_03_8 (unzipSignalBit8 sig))
  , Cons (_04_8 s) (_04_8 (unzipSignalBit8 sig))
  , Cons (_05_8 s) (_05_8 (unzipSignalBit8 sig))
  , Cons (_06_8 s) (_06_8 (unzipSignalBit8 sig))
  , Cons (_07_8 s) (_07_8 (unzipSignalBit8 sig))
  , Cons (_08_8 s) (_08_8 (unzipSignalBit8 sig))
  )

zipSignalBit8 ::
  (Signal Bit, Signal Bit, Signal Bit, Signal Bit, Signal Bit, Signal Bit, Signal Bit, Signal Bit)
  -> Signal Bit8
zipSignalBit8
  ( s01 `Cons` sig01 
  , s02 `Cons` sig02
  , s03 `Cons` sig03
  , s04 `Cons` sig04
  , s05 `Cons` sig05
  , s06 `Cons` sig06
  , s07 `Cons` sig07
  , s08 `Cons` sig08 ) =
  (s01,s02,s03,s04,s05,s06,s07,s08) `Cons` 
  (zipSignalBit8 (sig01,sig02,sig03,sig04,sig05,sig06,sig07,sig08))

unzipSignalBit16 :: 
  Signal Bit16 -> 
  (Signal Bit, Signal Bit, Signal Bit, Signal Bit, Signal Bit, Signal Bit, Signal Bit, Signal Bit, 
   Signal Bit, Signal Bit, Signal Bit, Signal Bit, Signal Bit, Signal Bit, Signal Bit, Signal Bit)
unzipSignalBit16 ~(s `Cons` sig) =
  ( Cons (_01_16 s) (_01_16 (unzipSignalBit16 sig))
  , Cons (_02_16 s) (_02_16 (unzipSignalBit16 sig))
  , Cons (_03_16 s) (_03_16 (unzipSignalBit16 sig))
  , Cons (_04_16 s) (_04_16 (unzipSignalBit16 sig))
  , Cons (_05_16 s) (_05_16 (unzipSignalBit16 sig))
  , Cons (_06_16 s) (_06_16 (unzipSignalBit16 sig))
  , Cons (_07_16 s) (_07_16 (unzipSignalBit16 sig))
  , Cons (_08_16 s) (_08_16 (unzipSignalBit16 sig))
  , Cons (_09_16 s) (_09_16 (unzipSignalBit16 sig))
  , Cons (_10_16 s) (_10_16 (unzipSignalBit16 sig))
  , Cons (_11_16 s) (_11_16 (unzipSignalBit16 sig))
  , Cons (_12_16 s) (_12_16 (unzipSignalBit16 sig))
  , Cons (_13_16 s) (_13_16 (unzipSignalBit16 sig))
  , Cons (_14_16 s) (_14_16 (unzipSignalBit16 sig))
  , Cons (_15_16 s) (_15_16 (unzipSignalBit16 sig))
  , Cons (_16_16 s) (_16_16 (unzipSignalBit16 sig))
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
