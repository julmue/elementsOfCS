{-# LANGUAGE  TypeSynonymInstances #-}
{-# LANGUAGE  FlexibleInstances #-}

module Logic (
    Bit(..),
    Bit2,
    Bit3,
    Bit4,
    Bit8,
    Bit16,
 
    replicateBit16,
    fmapBit16,
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

    mux16',
    _01,
    _02,
    _03,
    _04,
    _05,
    _06,
    _07,
    _08,
    _09,
    _10,
    _11,
    _12,
    _13,
    _14,
    _15,
    _16,
    fmap16',

    charToBit,
    stringToBit2,
    stringToBit8,
    stringToBit16
    ) where

import Prelude hiding (not, and, or)
import Data.List (intercalate)

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
dmux :: Bit -> Bit -> (Bit, Bit)
dmux p sel = let
    a = mux p O sel
    b = mux p O (not sel)
    in (a, b)

{- multi bit arrays "buses" -}
-- type Bit = Bool
data Bit = I | O deriving (Show, Read, Eq)


type Bit2 = (
    Bit, 
    Bit)

type Bit3 = (
    Bit,
    Bit,
    Bit)

type Bit4 = (
    Bit,
    Bit,
    Bit,
    Bit)

type Bit8 = (
    Bit, 
    Bit, 
    Bit, 
    Bit, 
   
    Bit, 
    Bit, 
    Bit, 
    Bit)
    
type Bit16 = (
    Bit, 
    Bit, 
    Bit, 
    Bit, 
   
    Bit, 
    Bit, 
    Bit, 
    Bit,
    
    Bit, 
    Bit, 
    Bit, 
    Bit, 
   
    Bit, 
    Bit, 
    Bit, 
    Bit)

instance Show Bit16 where 
    show (b01,b02,b03,b04,b05,b06,b07,b08,b09,b10,b11,b12,b13,b14,b15,b16) =
        mconcat . fmap show $ [b01,b02,b03,b04,b05,b06,b07,b08,b09,b10,b11,b12,b13,b14,b15,b16]

instance Eq Bit16 where
    (a01,a02,a03,a04,a05,a06,a07,a08,a09,a10,a11,a12,a13,a14,a15,a16) == (b01,b02,b03,b04,b05,b06,b07,b08,b09,b10,b11,b12,b13,b14,b15,b16) =
        all id [ a01 == b01
            , a02 == b02
            , a03 == b03
            , a04 == b04
            , a05 == b05
            , a06 == b06
            , a07 == b07
            , a08 == b08
            , a09 == b09
            , a10 == b10
            , a11 == b11
            , a12 == b12
            , a13 == b13
            , a14 == b14
            , a15 == b15
            , a16 == b16
            ]

replicateBit16 :: Bit -> Bit16
replicateBit16 b = (b,b,b,b,b,b,b,b,b,b,b,b,b,b,b,b)

fmapBit16 :: (Bit -> Bit) -> Bit16 -> Bit16
fmapBit16 f 
    (b01, b02, b03, b04, b05, b06, b07, b08, b09, b10, b11, b12, b13, b14, b15, b16) = 
    (f b01, f b02, f b03, f b04, f b05, f b06, f b07, f b08, f b09, f b10, f b11, f b12, f b13, f b14, f b15, f b16)

zipWithBit16 :: (Bit -> Bit -> Bit) -> Bit16 -> Bit16 -> Bit16
zipWithBit16 
    f     
    (a01, a02, a03, a04, a05, a06, a07, a08, a09, a10, a11, a12, a13, a14, a15, a16)
    (b01, b02, b03, b04, b05, b06, b07, b08, b09, b10, b11, b12, b13, b14, b15, b16) =
    ( f a01 b01
    , f a02 b02
    , f a03 b03
    , f a04 b04
    , f a05 b05
    , f a06 b06
    , f a07 b07
    , f a08 b08
    , f a09 b09
    , f a10 b10
    , f a11 b11
    , f a12 b12
    , f a13 b13
    , f a14 b14
    , f a15 b15
    , f a16 b16 ) 
 
zipWith3Bit16 :: (Bit -> Bit -> Bit -> Bit) -> Bit16 -> Bit16 -> Bit16 -> Bit16
zipWith3Bit16 
    f     
    (a01, a02, a03, a04, a05, a06, a07, a08, a09, a10, a11, a12, a13, a14, a15, a16)
    (b01, b02, b03, b04, b05, b06, b07, b08, b09, b10, b11, b12, b13, b14, b15, b16) 
    (c01, c02, c03, c04, c05, c06, c07, c08, c09, c10, c11, c12, c13, c14, c15, c16) =
    ( f a01 b01 b01
    , f a02 b02 c02
    , f a03 b03 c03
    , f a04 b04 c04
    , f a05 b05 c05
    , f a06 b06 c06
    , f a07 b07 c07
    , f a08 b08 c08
    , f a09 b09 c09
    , f a10 b10 c10
    , f a11 b11 c11
    , f a12 b12 c12
    , f a13 b13 c13
    , f a14 b14 c14
    , f a15 b15 c15
    , f a16 b16 c16 ) 

not16 :: Bit16 -> Bit16
not16 = fmapBit16 not

and16 :: Bit16 -> Bit16 -> Bit16
and16 = zipWithBit16 and

or16 :: Bit16 -> Bit16 -> Bit16
or16 = zipWithBit16 or

-- sel=0 -> a
-- sel=1 -> b
mux16 :: Bit16 -> Bit16 -> Bit -> Bit16
mux16 a16 b16 sel =
    let sel16 = replicateBit16 sel 
    in zipWith3Bit16 mux a16 b16 sel16

or8Way :: Bit8 -> Bit
or8Way (b01, b02, b03, b04, b05, b06, b07, b08) = 
    b01 `or` b02 `or` b03 `or` b04 `or` b05 `or` b06 `or` b07 `or` b08

-- sel=00 -> a
-- sel=01 -> b
-- sel=10 -> b
-- sel=11 -> d
mux4Way16 :: Bit16 -> Bit16 -> Bit16 -> Bit16 -> Bit2 -> Bit16
mux4Way16 a b c d sel@(sel1, sel2) = 
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
mux8Way16 a b c d e f g h (sel1, sel2, sel3) =
    let sel' = (sel2, sel3)
        x = mux4Way16 a b c d sel'
        y = mux4Way16 e f g h sel'
    in mux16 x y sel1 

-- sel=00 -> a=in, b=c=d=0
-- sel=01 -> b=in, a=c=d=0
-- sel=10 -> c=in, a=b=d=0
-- sel=11 -> d=in, a=b=c=0
dmux4Way :: Bit -> Bit2 -> Bit4
dmux4Way _in (sel1, sel2) = 
    let (x, y) = dmux _in sel1
        (out1, out2) = dmux x sel2
        (out3, out4) = dmux y sel2
    in (out1, out2, out3, out4)

-- sel=000 -> a=in
-- sel=001 -> b=in
-- sel=010 -> c=in
-- sel=011 -> d=in
-- sel=100 -> e=in
-- sel=101 -> f=in
-- sel=110 -> g=in
-- sel=111 -> h=in
dmux8Way :: Bit -> Bit3 -> Bit8
dmux8Way _in (sel1, sel2, sel3) =
    let sel' = (sel2, sel3)
        (x,y) = dmux _in sel1 
        (out1, out2, out3, out4) = dmux4Way x sel'
        (out5, out6, out7, out8) = dmux4Way y sel'
    in (out1, out2, out3, out4, out5, out6, out7, out8)

-- ----------------------------------------------------------------------------
-- helpers

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

fmap16' :: 
    (a -> b) 
    -> (a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a) 
    -> (b,b,b,b,b,b,b,b,b,b,b,b,b,b,b,b)
fmap16' f
    (b01,b02,b03,b04,b05,b06,b07,b08,b09,b10,b11,b12,b13,b14,b15,b16) = 
    (f b01,f b02,f b03,f b04,f b05,f b06,f b07,f b08,f b09,f b10,f b11,f b12,f b13,f b14,f b15,f b16)

-- ----------------------------------------------------------------------------
-- helpers

charToBit :: Char -> Bit
charToBit 'I' = I
charToBit 'O' = O

stringToBit2 :: String -> Bit2
stringToBit2 s = 
    case fmap charToBit s of 
        [b1,b2] -> (b1, b2)
        _ -> error "bit2"

stringToBit4 :: String -> Bit4
stringToBit4 s = 
    case fmap charToBit s of
        [b1,b2,b3,b4] -> (b1,b2,b3,b4)
        _ -> error "bit4"

stringToBit8 :: String -> Bit8
stringToBit8 s =
    case fmap charToBit s of 
        [b1,b2,b3,b4,b5,b6,b7,b8] -> (b1,b2,b3,b4,b5,b6,b7,b8)
        _ -> error "bit8"

stringToBit16 :: String -> Bit16
stringToBit16 s =
    case fmap charToBit s of 
        [b01,b02,b03,b04,b05,b06,b07,b08,b09,b10,b11,b12,b13,b14,b15,b16] -> 
            (b01,b02,b03,b04,b05,b06,b07,b08,b09,b10,b11,b12,b13,b14,b15,b16)
        _ -> error "bit16"