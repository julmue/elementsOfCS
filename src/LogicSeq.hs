{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}

module LogicSeq ( 
   dff
  , bit
  , register
  , ram8
  , ram64
  , ram512
  , ram4K
  , ram16K
  , ram32K
  ) where

import Vector
import Logic
import Arithmetic

import Prelude hiding (iterate, take)
import Text.Pretty.Simple (pPrint)
-- import Text.Show.Pretty -- needs generics ... try out later

-- maybe rewrite mtl style
import Control.Monad.State

type Byte16 = V8 Bit16
type Byte128 = V8 Byte16
type KByte1 = V8 Byte128
type KByte8 = V8 KByte1
type KByte16 = V4 KByte8
type KByte32 = V8 KByte8

run :: (in_ -> load -> State s out) -> in_ -> load -> s -> (out, s)
run component in_ load initial = runState (component in_ load) initial

run' :: (in_ -> addr -> load -> State s out) -> in_ -> addr -> load -> s -> (out, s)
run' component in_ addr load initial = runState (component in_ addr load) initial

dff :: Bit -> State Bit Bit
dff in_ = do
  memory <- get
  put in_
  return memory

bit :: Bit -> Bit -> State Bit Bit
bit in_ load = mfix (\out -> dff (mux out in_ load))

register :: Bit16 -> Bit -> State Bit16 Bit16
register in_ load = do
  (V16 b01 b02 b03 b04 b05 b06 b07 b08 b09 b10 b11 b12 b13 b14 b15 b16) <- get
  let (V16 i01 i02 i03 i04 i05 i06 i07 i08 i09 i10 i11 i12 i13 i14 i15 i16) = in_
      (o01, b01') = run bit i01 load b01
      (o02, b02') = run bit i02 load b02 
      (o03, b03') = run bit i03 load b03 
      (o04, b04') = run bit i04 load b04 
      (o05, b05') = run bit i05 load b05 
      (o06, b06') = run bit i06 load b06 
      (o07, b07') = run bit i07 load b07 
      (o08, b08') = run bit i08 load b08 
      (o09, b09') = run bit i09 load b09 
      (o10, b10') = run bit i10 load b10 
      (o11, b11') = run bit i11 load b11 
      (o12, b12') = run bit i12 load b12
      (o13, b13') = run bit i13 load b13
      (o14, b14') = run bit i14 load b14
      (o15, b15') = run bit i15 load b15
      (o16, b16') = run bit i16 load b16 
  put (V16 b01' b02' b03' b04' b05' b06' b07' b08' b09' b10' b11' b12' b13' b14' b15' b16')
  return (V16 o01 o02 o03 o04 o05 o06 o07 o08 o09 o10 o11 o12 o13 o14 o15 o16)

ram8 :: Bit16 -> Bit3 -> Bit -> State Byte16 Bit16
ram8 in_ addr load = do
  (V8 r01 r02 r03 r04 r05 r06 r07 r08) <- get
  let (V8 l01 l02 l03 l04 l05 l06 l07 l08) = dmux8Way load addr
      (o01, r01') = run register in_ l01 r01
      (o02, r02') = run register in_ l02 r02 
      (o03, r03') = run register in_ l03 r03 
      (o04, r04') = run register in_ l04 r04 
      (o05, r05') = run register in_ l05 r05 
      (o06, r06') = run register in_ l06 r06 
      (o07, r07') = run register in_ l07 r07 
      (o08, r08') = run register in_ l08 r08 
  put (V8 r01' r02' r03' r04' r05' r06' r07' r08')
  return (mux8Way16 o01 o02 o03 o04 o05 o06 o07 o08 addr)

ram64 :: Bit16 -> Bit6 -> Bit -> State Byte128 Bit16
ram64 in_ addr load = do
  (V8 r01 r02 r03 r04 r05 r06 r07 r08) <- get
  let (V6 a01 a02 a03 a04 a05 a06) = addr
      addr' = (V3 a01 a02 a03)
      addr'' = (V3 a04 a05 a06)
      (V8 l01 l02 l03 l04 l05 l06 l07 l08) = dmux8Way load addr'
      (o01, r01') = run' ram8 in_ addr'' l01 r01
      (o02, r02') = run' ram8 in_ addr'' l02 r02 
      (o03, r03') = run' ram8 in_ addr'' l03 r03 
      (o04, r04') = run' ram8 in_ addr'' l04 r04 
      (o05, r05') = run' ram8 in_ addr'' l05 r05 
      (o06, r06') = run' ram8 in_ addr'' l06 r06 
      (o07, r07') = run' ram8 in_ addr'' l07 r07 
      (o08, r08') = run' ram8 in_ addr'' l08 r08 
  put (V8 r01' r02' r03' r04' r05' r06' r07' r08')
  return (mux8Way16 o01 o02 o03 o04 o05 o06 o07 o08 addr')

ram512 :: Bit16 -> Bit9 -> Bit -> State KByte1 Bit16
ram512 in_ addr load = do
  (V8 r01 r02 r03 r04 r05 r06 r07 r08) <- get
  let (V9 a01 a02 a03 a04 a05 a06 a07 a08 a09) = addr
      addr' = (V3 a01 a02 a03)
      addr'' = (V6 a04 a05 a06 a07 a08 a09)
      (V8 l01 l02 l03 l04 l05 l06 l07 l08) = dmux8Way load addr'
      (o01, r01') = run' ram64 in_ addr'' l01 r01
      (o02, r02') = run' ram64 in_ addr'' l02 r02 
      (o03, r03') = run' ram64 in_ addr'' l03 r03 
      (o04, r04') = run' ram64 in_ addr'' l04 r04 
      (o05, r05') = run' ram64 in_ addr'' l05 r05 
      (o06, r06') = run' ram64 in_ addr'' l06 r06 
      (o07, r07') = run' ram64 in_ addr'' l07 r07 
      (o08, r08') = run' ram64 in_ addr'' l08 r08 
  put (V8 r01' r02' r03' r04' r05' r06' r07' r08')
  return (mux8Way16 o01 o02 o03 o04 o05 o06 o07 o08 addr')

ram4K :: Bit16 -> Bit12 -> Bit -> State KByte8 Bit16
ram4K in_ addr load = do
  (V8 r01 r02 r03 r04 r05 r06 r07 r08) <- get
  let (V12 a01 a02 a03 a04 a05 a06 a07 a08 a09 a10 a11 a12) = addr
      addr' = (V3 a01 a02 a03)
      addr'' = (V9 a04 a05 a06 a07 a08 a09 a10 a11 a12)
      (V8 l01 l02 l03 l04 l05 l06 l07 l08) = dmux8Way load addr'
      (o01, r01') = run' ram512 in_ addr'' l01 r01
      (o02, r02') = run' ram512 in_ addr'' l02 r02 
      (o03, r03') = run' ram512 in_ addr'' l03 r03 
      (o04, r04') = run' ram512 in_ addr'' l04 r04 
      (o05, r05') = run' ram512 in_ addr'' l05 r05 
      (o06, r06') = run' ram512 in_ addr'' l06 r06 
      (o07, r07') = run' ram512 in_ addr'' l07 r07 
      (o08, r08') = run' ram512 in_ addr'' l08 r08 
  put (V8 r01' r02' r03' r04' r05' r06' r07' r08')
  return (mux8Way16 o01 o02 o03 o04 o05 o06 o07 o08 addr')

ram16K :: Bit16 -> Bit14 -> Bit -> State KByte16 Bit16
ram16K in_ addr load = do
  (V4 r01 r02 r03 r04) <- get
  let (V14 a01 a02 a03 a04 a05 a06 a07 a08 a09 a10 a11 a12 a13 a14) = addr
      addr' = (V2 a01 a02)
      addr'' = (V12 a03 a04 a05 a06 a07 a08 a09 a10 a11 a12 a13 a14)
      (V4 l01 l02 l03 l04) = dmux4Way load addr'
      (o01, r01') = run' ram4K in_ addr'' l01 r01
      (o02, r02') = run' ram4K in_ addr'' l02 r02 
      (o03, r03') = run' ram4K in_ addr'' l03 r03 
      (o04, r04') = run' ram4K in_ addr'' l04 r04 
  put (V4 r01' r02' r03' r04')
  return (mux4Way16 o01 o02 o03 o04 addr')

ram32K :: Bit16 -> Bit15 -> Bit -> State KByte32 Bit16
ram32K in_ addr load = do
  (V8 r01 r02 r03 r04 r05 r06 r07 r08) <- get
  let (V15 a01 a02 a03 a04 a05 a06 a07 a08 a09 a10 a11 a12 a13 a14 a15) = addr
      addr' = (V3 a01 a02 a03)
      addr'' = (V12 a04 a05 a06 a07 a08 a09 a10 a11 a12 a13 a14 a15)
      (V8 l01 l02 l03 l04 l05 l06 l07 l08) = dmux8Way load addr'
      (o01, r01') = run' ram4K in_ addr'' l01 r01
      (o02, r02') = run' ram4K in_ addr'' l02 r02 
      (o03, r03') = run' ram4K in_ addr'' l03 r03 
      (o04, r04') = run' ram4K in_ addr'' l04 r04 
      (o05, r05') = run' ram4K in_ addr'' l05 r05 
      (o06, r06') = run' ram4K in_ addr'' l06 r06 
      (o07, r07') = run' ram4K in_ addr'' l07 r07 
      (o08, r08') = run' ram4K in_ addr'' l08 r08 
  put (V8 r01' r02' r03' r04' r05' r06' r07' r08')
  return (mux8Way16 o01 o02 o03 o04 o05 o06 o07 o08 addr')