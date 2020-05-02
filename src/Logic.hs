{-# LANGUAGE  TypeSynonymInstances #-}
{-# LANGUAGE  FlexibleInstances #-}

module Logic (
    Bit(..),
    Bit2,
    Bit3,
    Bit4,
    Bit6,
    Bit8,
    Bit9,
    Bit12,
    Bit14,
    Bit16,

    zipWithBit16,
    zipWith3Bit16,
    
    not,
    and,
    or,
    xor,
    mux,
    dmux,

    not16,
    and16,
    or16,
    mux16,
    or8Way,

    mux4Way16,
    mux8Way16,

    dmux4Way,
    dmux8Way,

    mux8',
    mux16'
,

    charToBit,
    stringToBit2,
    stringToBit8,
    stringToBit16
    ) where

import Prelude hiding (not, and, or)
import Data.List (intercalate)
import Text.Read

import Vector

nand :: Bit -> Bit -> Bit
nand I I = O
nand I O = I
nand O I = I
nand O O = I

not :: Bit -> Bit
not a = nand a a

and :: Bit -> Bit -> Bit
and a b = not (nand a b)

or :: Bit -> Bit -> Bit
or a b = nand (not a) (not b)

xor :: Bit -> Bit -> Bit
xor a b = ((not a) `and` b) `or` (a `and` (not b)) 

-- if sel=O then out=a else out=b
mux :: Bit -> Bit -> Bit -> Bit
mux a b sel = (a `and` (not sel)) `or` (b `and` sel) 

-- if sel=0 
-- then (a=p, b=O)
-- else (a=O, b=p)
dmux :: Bit -> Bit -> Bit2
dmux p sel = let
    a = mux p O sel
    b = mux p O (not sel)
    in V2 a b

{- multi bit arrays "buses" -}
-- type Bit = Bool
data Bit = I | O deriving (Show, Read, Eq)

type Bit2 = V2 Bit
instance {-# Overlapping #-} Show Bit2 where 
    show (V2 b1 b2) = 
        show b1 <> show b2 

type Bit3 = V3 Bit
instance {-# Overlapping #-} Show Bit3 where 
    show (V3 b1 b2 b3) = 
        show b1 <> show b2 <> show b3 

type Bit4 = V4 Bit
instance {-# Overlapping #-} Show Bit4 where 
    show (V4 b1 b2 b3 b4) = 
        show b1 <> show b2 <> show b3 <> show b4 

type Bit5 = V5 Bit
instance {-# Overlapping #-} Show Bit5 where 
    show (V5 b1 b2 b3 b4 b5) = 
        show b1 <> show b2 <> show b3 <> show b4 <> show b5 

type Bit6 = V6 Bit
instance {-# Overlapping #-} Show Bit6 where 
    show (V6 b1 b2 b3 b4 b5 b6) = 
        show b1 <> show b2 <> show b3 <> show b4 <> show b5 <> show b6

type Bit8 = V8 Bit
instance {-# Overlapping #-} Show Bit8 where 
    show (V8 b1 b2 b3 b4 b5 b6 b7 b8) = 
        show b1 <> show b2 <> show b3 <> show b4 <> show b5 <> show b6 <> show b7 <> show b8

type Bit9 = V9 Bit

type Bit12 = V12 Bit

type Bit14 = V14 Bit

type Bit16 = V16 Bit
instance {-# Overlapping #-} Show Bit16 where 
    show (V16 b01 b02 b03 b04 b05 b06 b07 b08 b09 b10 b11 b12 b13 b14 b15 b16) = 
        show b01 <> show b02 <> show b03 <> show b04 <> show b05 <> show b06 <> show b07 <> show b08 <>
        show b09 <> show b10 <> show b11 <> show b12 <> show b13 <> show b14 <> show b15 <> show b16

-- instance Show Bit16 where 
--     show (b01,b02,b03,b04,b05,b06,b07,b08,b09,b10,b11,b12,b13,b14,b15,b16) =
--         mconcat . fmap show $ [b01,b02,b03,b04,b05,b06,b07,b08,b09,b10,b11,b12,b13,b14,b15,b16]

zipWithBit16 :: (Bit -> Bit -> Bit) -> Bit16 -> Bit16 -> Bit16
zipWithBit16 
    f     
    (V16 a01 a02 a03 a04 a05 a06 a07 a08 a09 a10 a11 a12 a13 a14 a15 a16)
    (V16 b01 b02 b03 b04 b05 b06 b07 b08 b09 b10 b11 b12 b13 b14 b15 b16) =
    V16 (f a01 b01)
        (f a02 b02)
        (f a03 b03)
        (f a04 b04)
        (f a05 b05)
        (f a06 b06)
        (f a07 b07)
        (f a08 b08)
        (f a09 b09)
        (f a10 b10)
        (f a11 b11)
        (f a12 b12)
        (f a13 b13)
        (f a14 b14)
        (f a15 b15)
        (f a16 b16) 
 
zipWith3Bit16 :: (Bit -> Bit -> Bit -> Bit) -> Bit16 -> Bit16 -> Bit16 -> Bit16
zipWith3Bit16 
    f     
    (V16 a01 a02 a03 a04 a05 a06 a07 a08 a09 a10 a11 a12 a13 a14 a15 a16)
    (V16 b01 b02 b03 b04 b05 b06 b07 b08 b09 b10 b11 b12 b13 b14 b15 b16)
    (V16 c01 c02 c03 c04 c05 c06 c07 c08 c09 c10 c11 c12 c13 c14 c15 c16) =
    V16 (f a01 b01 b01)
        (f a02 b02 c02)
        (f a03 b03 c03)
        (f a04 b04 c04)
        (f a05 b05 c05)
        (f a06 b06 c06)
        (f a07 b07 c07)
        (f a08 b08 c08)
        (f a09 b09 c09)
        (f a10 b10 c10)
        (f a11 b11 c11)
        (f a12 b12 c12)
        (f a13 b13 c13)
        (f a14 b14 c14)
        (f a15 b15 c15)
        (f a16 b16 c16)

not16 :: Bit16 -> Bit16
not16 = fmap not

and16 :: Bit16 -> Bit16 -> Bit16
and16 = zipWithBit16 and

or16 :: Bit16 -> Bit16 -> Bit16
or16 = zipWithBit16 or

-- sel=0 -> a
-- sel=1 -> b
mux16 :: Bit16 -> Bit16 -> Bit -> Bit16
mux16 a16 b16 sel =
    let sel16 = replicateV16 sel 
    in zipWith3Bit16 mux a16 b16 sel16

or8Way :: Bit8 -> Bit
or8Way (V8 b1 b2 b3 b4 b5 b6 b7 b8) = 
    b1 `or` b2 `or` b3 `or` b4 `or` b5 `or` b6 `or` b7 `or` b8

-- sel=00 -> a
-- sel=01 -> b
-- sel=10 -> b
-- sel=11 -> d
mux4Way16 :: Bit16 -> Bit16 -> Bit16 -> Bit16 -> Bit2 -> Bit16
mux4Way16 a b c d (V2 sel1 sel2) = 
    let x = mux16 a b sel2 
        y = mux16 c d sel2 
    in mux16 x y sel1

-- sel=000 -> a
-- sel=001 -> b
-- sel=010 -> c
-- sel=011 -> d
-- sel=100 -> e
-- sel=101 -> f
-- sel=110 -> g
-- sel=111 -> h

mux8Way16 :: Bit16 -> Bit16 -> Bit16 -> Bit16 -> Bit16 -> Bit16 -> Bit16 -> Bit16 ->
             Bit3 -> Bit16
mux8Way16 a b c d e f g h sel@(V3 sel1 sel2 sel3) =
    let sel' = V2 sel2 sel3
        x = mux4Way16 a b c d sel'
        y = mux4Way16 e f g h sel'
    in mux16 x y sel1 

-- sel=00 -> a=in, b=c=d=0
-- sel=01 -> b=in, a=c=d=0
-- sel=10 -> c=in, a=b=d=0
-- sel=11 -> d=in, a=b=c=0
dmux4Way :: Bit -> Bit2 -> Bit4
dmux4Way _in (V2 sel1 sel2) = 
    let (V2 x y) = dmux _in sel1
        (V2 out1 out2) = dmux x sel2
        (V2 out3 out4) = dmux y sel2
    in V4 out1 out2 out3 out4

-- sel=000 -> a=in
-- sel=001 -> b=in
-- sel=010 -> c=in
-- sel=011 -> d=in
-- sel=100 -> e=in
-- sel=101 -> f=in
-- sel=110 -> g=in
-- sel=111 -> h=in
dmux8Way :: Bit -> Bit3 -> Bit8
dmux8Way _in (V3 sel1 sel2 sel3) =
    let sel' = V2 sel2 sel3
        V2 x y = dmux _in sel1 
        (V4 out1 out2 out3 out4) = dmux4Way x sel'
        (V4 out5 out6 out7 out8) = dmux4Way y sel'
    in V8 out1 out2 out3 out4 out5 out6 out7 out8

-- ----------------------------------------------------------------------------
-- helpers

mux8' :: (a,a,a,a,a,a,a,a) -> Bit -> Bit -> Bit -> a
mux8' ~(b01,b02,b03,b04,b05,b06,b07,b08) sel1 sel2 sel3 =
  case (sel1, sel2, sel3) of
    (O,O,O) -> b01
    (O,O,I) -> b02
    (O,I,O) -> b03
    (O,I,I) -> b04
    (I,O,O) -> b05
    (I,O,I) -> b06
    (I,I,O) -> b07
    (I,I,I) -> b08

_01_8 b16 = mux8' b16 O O O
_02_8 b16 = mux8' b16 O O I
_03_8 b16 = mux8' b16 O I O
_04_8 b16 = mux8' b16 O I I
_05_8 b16 = mux8' b16 I O O
_06_8 b16 = mux8' b16 I O I
_07_8 b16 = mux8' b16 I I O
_08_8 b16 = mux8' b16 I I I

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

-- ----------------------------------------------------------------------------
-- helpers

charToBit :: Char -> Bit
charToBit 'I' = I
charToBit 'O' = O

stringToBit2 :: String -> Bit2
stringToBit2 s = 
    case fmap charToBit s of 
        [b1,b2] -> V2 b1 b2
        _ -> error "bit2"

stringToBit4 :: String -> Bit4
stringToBit4 s = 
    case fmap charToBit s of
        [b1,b2,b3,b4] -> V4 b1 b2 b3 b4
        _ -> error "bit4"

stringToBit8 :: String -> Bit8
stringToBit8 s =
    case fmap charToBit s of 
        [b1,b2,b3,b4,b5,b6,b7,b8] -> V8 b1 b2 b3 b4 b5 b6 b7 b8
        _ -> error "bit8"

stringToBit16 :: String -> Bit16
stringToBit16 s =
    case fmap charToBit s of 
        [b01,b02,b03,b04,b05,b06,b07,b08,b09,b10,b11,b12,b13,b14,b15,b16] -> 
            V16 b01 b02 b03 b04 b05 b06 b07 b08 b09 b10 b11 b12 b13 b14 b15 b16
        _ -> error "bit16"