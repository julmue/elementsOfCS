module Arithmetic (
      halfAdder
    , fullAdder
) where

import Prelude(undefined)
import Logic
import Data.Bool (Bool(..))

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
    in (carry, sum)

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
    let (carry, sum) = halfAdder a b
        (carry', sum') = halfAdder c sum
    in (carry' `or` carry, sum')
