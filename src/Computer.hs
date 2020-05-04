module Computer where 

import Vector
import Logic
import Arithmetic
import LogicSeq

import Control.Monad.State

type MemoryIn = Bit16
type MemoryWrite = Bit
type Address = Bit15

rom32K :: Bit15 -> Bit16
rom32K addr = undefined

-- maybe we have to supply the address for the instruction too
-- we can use ram32k for the rom
flashRom :: [Bit16] -> Bit15 -> Bit16
flashRom = undefined
 
memory :: Bit16 -> Bit15 -> Bit -> State KByte32 Bit16
memory = ram32K

-- screen = undefined

-- keyboard :: a
-- keyboard = undefined

-- memory is not part of the cpu
--     _memory :: KByte32,
data CpuState = CpuState {
    _A :: Bit16,
    _D :: Bit16,
    _PC :: Bit16
    } deriving (Show, Read, Eq)

cpu :: Bit16 -> Bit16 -> Bit -> State CpuState (Bit16, Bit, Address, Address) 
cpu inM instruction reset =
    let outM = undefined
        writeM = undefined 
        addressM = undefined
        pc = undefined
    in return (outM, writeM, addressM, pc)

cpuControl :: Bit16 -> a
cpuControl instruction = undefined