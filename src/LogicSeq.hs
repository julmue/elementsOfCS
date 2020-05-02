{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}

module LogicSeq ( 
--    dff
--  , bit
--  , register
--  , Signal
  ) where

import Vector
import Logic
import Arithmetic

import Prelude hiding (iterate, take)

import Data.Stream (Stream(..), (<:>))
import qualified Data.Stream as S
import Control.Monad.State
import Control.Monad

-- Signal = Stream Bit?
type Signal = Stream

type Byte16 = V8 Bit16
type Byte128 = V8 Byte16
type KByte1 = V8 Byte128
type KByte8 = V8 KByte1
type KByte64 = V8 KByte8

dff_ :: Bit -> State Bit Bit
dff_ in_ = do
  memory <- get
  put in_
  return memory

bit_ :: Bit -> Bit -> State Bit Bit
bit_ in_ load = mfix (\out -> dff_ (mux out in_ load))

run :: (t1 -> t2 -> State s a) -> t1 -> t2 -> s -> (a, s)
run component in_ load initial = runState (component in_ load) initial

run' component in_ addr load initial = runState (component in_ addr load) initial

register_ :: Bit16 -> Bit -> State Bit16 Bit16
register_ i load = do
  (V16 b01 b02 b03 b04 b05 b06 b07 b08 b09 b10 b11 b12 b13 b14 b15 b16) <- get
  let (V16 i01 i02 i03 i04 i05 i06 i07 i08 i09 i10 i11 i12 i13 i14 i15 i16) = i
      (o01, b01') = run bit_ i01 load b01
      (o02, b02') = run bit_ i02 load b02 
      (o03, b03') = run bit_ i03 load b03 
      (o04, b04') = run bit_ i04 load b04 
      (o05, b05') = run bit_ i05 load b05 
      (o06, b06') = run bit_ i06 load b06 
      (o07, b07') = run bit_ i07 load b07 
      (o08, b08') = run bit_ i08 load b08 
      (o09, b09') = run bit_ i09 load b09 
      (o10, b10') = run bit_ i10 load b10 
      (o11, b11') = run bit_ i11 load b11 
      (o12, b12') = run bit_ i12 load b12
      (o13, b13') = run bit_ i13 load b13
      (o14, b14') = run bit_ i14 load b14
      (o15, b15') = run bit_ i15 load b15
      (o16, b16') = run bit_ i16 load b16 
  put (V16 b01' b02' b03' b04' b05' b06' b07' b08' b09' b10' b11' b12' b13' b14' b15' b16')
  return (V16 o01 o02 o03 o04 o05 o06 o07 o08 o09 o10 o11 o12 o13 o14 o15 o16)

ram8 :: Bit16 -> Bit3 -> Bit -> State Byte16 Bit16
ram8 i addr load = do
  (V8 r01 r02 r03 r04 r05 r06 r07 r08) <- get
  let (V8 l01 l02 l03 l04 l05 l06 l07 l08) = dmux8Way load addr
      (o01, r01') = run register_ i l01 r01
      (o02, r02') = run register_ i l02 r02 
      (o03, r03') = run register_ i l03 r03 
      (o04, r04') = run register_ i l04 r04 
      (o05, r05') = run register_ i l05 r05 
      (o06, r06') = run register_ i l06 r06 
      (o07, r07') = run register_ i l07 r07 
      (o08, r08') = run register_ i l08 r08 
  put (V8 r01' r02' r03' r04' r05' r06' r07' r08')
  return (mux8Way16 o01 o02 o03 o04 o05 o06 o07 o08 addr)

ram64 :: Bit16 -> Bit6 -> Bit -> State Byte128 Bit16
ram64 i addr load = do
  (V8 r01 r02 r03 r04 r05 r06 r07 r08) <- get
  let (V6 a01 a02 a03 a04 a05 a06) = addr
      addr' = (V3 a01 a02 a03)
      addr'' = (V3 a04 a05 a06)
      (V8 l01 l02 l03 l04 l05 l06 l07 l08) = dmux8Way load addr'
      (o01, r01') = run' ram8 i addr'' l01 r01
      (o02, r02') = run' ram8 i addr'' l02 r02 
      (o03, r03') = run' ram8 i addr'' l03 r03 
      (o04, r04') = run' ram8 i addr'' l04 r04 
      (o05, r05') = run' ram8 i addr'' l05 r05 
      (o06, r06') = run' ram8 i addr'' l06 r06 
      (o07, r07') = run' ram8 i addr'' l07 r07 
      (o08, r08') = run' ram8 i addr'' l08 r08 
  put (V8 r01' r02' r03' r04' r05' r06' r07' r08')
  return (mux8Way16 o01 o02 o03 o04 o05 o06 o07 o08 addr')

-- ram512 :: Signal Bit16 -> Signal Bit9 -> Signal Bit -> State KByte1 (Signal Bit16)
-- ram512 = undefined

-- ram4K :: Signal Bit16 -> Signal Bit12 -> Signal Bit -> State KByte8 (Signal Bit16)
-- ram4K = undefined

-- ram16K :: Signal Bit16 -> Signal Bit14 -> Signal Bit -> State KByte64 (Signal Bit16)
-- ram16K = undefined


_O16 :: Bit16
_O16 = stringToBit16 "OOOOOOOOOOOOOOOO"

_I16 :: Bit16
_I16 = stringToBit16 "IIIIIIIIIIIIIIII"

_OByte16 :: Byte16
_OByte16 = replicateV8 _O16

_OByte128 :: Byte128
_OByte128 = replicateV8 _OByte16


-- o16 :: Signal Bit16
-- o16 = fmap replicateBit16 o

-- clock :: Signal Bit
-- clock = S.interleave i o

-- test :: Signal Bit -> [Bit]
-- test signal = S.take 10 signal

-- run :: s -> State s a -> a
-- run = flip evalState
