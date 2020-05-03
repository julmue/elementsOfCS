module Arithmetic (
      halfAdder
    , fullAdder
) where

import Prelude hiding (Bool, all, any, and, or, not)
import Logic
import Vector

-- LSB = least significant bit
-- MSB = most significant bit
-- carry bit = remainder

-- 2's complement method

-- add two bits
-- a b    carry sum
-- 0 0 -> 0     0
-- 0 1 -> 0     1
-- 1 0 -> 0     1
-- 1 1 -> 1     0

halfAdder :: Bit -> Bit -> Bit2
halfAdder a b = 
    let carry = a `and` b
        sum = a `xor` b  
    in V2 carry sum

-- add three bits
-- add two bits
-- a b c    carry sum
-- 0 0 0 -> 0     0
-- 0 0 1 -> 0     1
-- 0 1 0 -> 0     1
-- 0 1 1 -> 1     0
-- 1 0 0 -> 0     1
-- 1 0 1 -> 1     0
-- 1 1 0 -> 1     0
-- 1 1 1 -> 1     1

fullAdder :: Bit -> Bit -> Bit -> Bit2
fullAdder a b c = 
    let (V2 carry sum) = halfAdder a b
        (V2 carry' sum') = halfAdder c sum
    in V2 (carry' `or` carry) sum'

add16 :: Bit16 -> Bit16 -> Bit16
add16 (V16 a16 a15 a14 a13 a12 a11 a10 a09 a08 a07 a06 a05 a04 a03 a02 a01)
      (V16 b16 b15 b14 b13 b12 b11 b10 b09 b08 b07 b06 b05 b04 b03 b02 b01) =
    let (V2 c01 s01) = halfAdder a01 b01
        (V2 c02 s02) = fullAdder a02 b02 c01
        (V2 c03 s03) = fullAdder a03 b03 c02
        (V2 c04 s04) = fullAdder a04 b04 c03
        (V2 c05 s05) = fullAdder a05 b05 c04
        (V2 c06 s06) = fullAdder a06 b06 c05
        (V2 c07 s07) = fullAdder a07 b07 c06
        (V2 c08 s08) = fullAdder a08 b08 c07
        (V2 c09 s09) = fullAdder a09 b09 c08
        (V2 c10 s10) = fullAdder a10 b10 c09
        (V2 c11 s11) = fullAdder a11 b11 c10
        (V2 c12 s12) = fullAdder a12 b12 c11
        (V2 c13 s13) = fullAdder a13 b13 c12
        (V2 c14 s14) = fullAdder a14 b14 c13
        (V2 c15 s15) = fullAdder a15 b15 c14
        (V2 _ s16) = fullAdder a16 b16 c15
    in (V16 s16 s15 s14 s13 s12 s11 s10 s09 s08 s07 s06 s05 s04 s03 s02 s01)


inc16 :: Bit16 -> Bit16
inc16 b = add16 b (V16 O O O O O O O O O O O O O O O I)

flip16 :: Bit16 -> Bit16
flip16 = fmap not

-- to obtain the code of -x from x
-- leave all the trailing (least significant) 0s
-- and the first least significant I intact
-- then flip the remaining bits
-- hardware shortcut: 
-- flip all the bits of x
-- add 1 to the result
negate16 :: Bit16 -> Bit16
negate16 = inc16 . flip16 

isNegative :: Bit16 -> Bit
isNegative (V16 msb _ _ _ _ _ _ _ _ _ _ _ _ _ _ _) = eq msb I 

eq16 :: Bit16 -> Bit16 -> Bit
eq16 a b = and16Way (zipWithV16 eq a b)

and16Way :: Bit16 -> Bit
and16Way (V16 a16 a15 a14 a13 a12 a11 a10 a09 a08 a07 a06 a05 a04 a03 a02 a01) =
    a16 `and` a15 `and` a14 `and` a13 `and` a12 `and` a11 `and` a10 `and` a09 `and` 
    a08 `and` a07 `and` a06 `and` a05 `and` a04 `and` a03 `and` a02 `and` a01

eq :: Bit -> Bit -> Bit
eq a b = (a `and` b) `or` (not a `and` not b)

-- overflow is neither detected nor handled
alu :: Bit16 -> Bit16 -> 
       Bit ->
       Bit -> 
       Bit -> 
       Bit -> 
       Bit -> 
       Bit -> 
       (Bit16, Bit, Bit)
alu x y zx nx zy ny f no = 
    let x' = mux16 x zero zx
        x'' = mux16 x' (negate16 x') nx
        y' = mux16 y zero zy
        y'' = mux16 y' (negate16 y') nx
        out = mux16 (add16 x'' y'') (and16 x'' y'') f
        out' = mux16 out (negate16 out) no
        zr = eq16 out' zero
        nr = isNegative out'
    in (out', zr, nr)
    where 
        zero = replicateV16 O

-- ----------------------------------------------------------------------------
-- helpers

binary1ToInt :: Bit -> Int
binary1ToInt O = 0
binary1ToInt I = 1

intToBinary1 :: Int -> Bit
intToBinary1 0 = O
intToBinary1 1 = I

f :: Int -> (Int, Int)
f i = (quot i 2, mod i 2) 

g :: Int -> [Int]
g i = case f i of
    (0, r) -> [r]
    (q, r) -> g q ++ [r]

unsignedIntToBinary8 :: Int -> Bit8
unsignedIntToBinary8 i = 
    let rs = fmap intToBinary1 . take 8 $ (g i) 
        -- should be the last n elments of the list ...
        fill = replicate (8 - length rs) O
        [b7, b6, b5, b4, b3, b2, b1, b0] = fill ++ rs
    in V8 b7 b6 b5 b4 b3 b2 b1 b0

-- should be unsigned?
binary8ToUnsignedInt :: Bit8 -> Int
binary8ToUnsignedInt (V8 b8 b7 b6 b5 b4 b3 b2 b1) =
    int b8 * 2^7 + 
    int b7 * 2^6 + 
    int b6 * 2^5 + 
    int b5 * 2^4 + 
    int b4 * 2^3 + 
    int b3 * 2^2 + 
    int b2 * 2^1 + 
    int b1 * 2^0
    where 
        int I = 1
        int O = 0

signedIntToBinary8 :: Int -> Bit8
signedIntToBinary8 0 = unsignedIntToBinary8 0
signedIntToBinary8 i = unsignedIntToBinary8 $ 2^8 - i 
