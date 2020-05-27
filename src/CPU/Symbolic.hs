module CPU.Symbolic where

import Vector
import Logic
import Arithmetic
import LogicSeq
import Language

import Control.Monad.State
import Debug.Trace

import Data.Int (Int16)
import Data.Bits (complement, (.&.), (.|.))

import Language

data CpuIn = CpuIn {
      _valueIn :: Int16,
      _instruction :: SymInstr,
      _reset :: Bool
    } deriving (Show, Eq)

data CpuState = CpuState {
      _A :: Int16,
      _D :: Int16,
      _PC :: Int16
    } deriving (Show, Eq)

data CpuOut = CpuOut {
      _valueOut :: Int16,           -- value out
      _writeToMem :: Bool,           -- write value to memory 
      _memoryAddr :: Int16,         -- value memory address; only 15 lsb relevant
      _programCounter :: Int16,     -- address of next instruction; only 15 lsb relevant
      _halt :: Bool                
    } deriving (Show, Eq)

-- rewrite this usind lenses
setA :: Int16 -> State CpuState ()
setA v = do
  (CpuState a d pc) <- get
  put (CpuState v d pc)
setD :: Int16 -> State CpuState ()
setD v = do
  (CpuState a d pc) <- get
  put (CpuState a v pc)
setPC :: Int16 -> State CpuState ()
setPC v = do
  (CpuState a d pc) <- get
  put (CpuState a d v)
incPC :: State CpuState Int16
incPC = do
  (CpuState a d pc) <- get
  let pc' = pc + 1
  put (CpuState a d pc')
  return pc'

-- a instruction
-- sets a register to 15 bit value

cpu :: CpuIn -> State CpuState CpuOut
cpu (CpuIn v ins _) = case ins of
  (SymA (SymValue v)) -> setA v >> incPC >>= \pc' -> return (CpuOut 0 False v pc' False)
  (SymC (SymInstrC cp ds jm)) -> do
    (CpuState a d pc) <- get
    let v' = comp cp v a d
    (outVal, writeM) <- dest ds v'
    if jump jm v'
      then undefined
      else undefined

    

-- this is just for c instructions ...
-- and this is the stuff rooted to the alu
comp :: SymComp -> Int16 -> Int16 -> Int16 ->  Int16
comp c m a d =
    case c of
        C_1 -> 1
        C_0 -> 0
        C_NEG_1 -> (-1)
        C_A -> a
        C_D -> d
        C_NOT_D -> complement d
        C_NOT_A -> complement a
        C_NEG_D -> negate d
        C_NEG_A -> negate a
        C_INC_D -> d + 1
        C_INC_A -> a + 1
        C_DEC_D -> d - 1
        C_DEC_A -> a - 1
        C_ADD_D_A -> d + a
        C_SUB_D_A -> d - a
        C_SUB_A_D -> a - d
        C_AND_D_A -> d .&. a 
        C_OR_D_A -> d .|. a
        C_M -> m  
        C_NOT_M -> complement m 
        C_NEG_M -> negate m 
        C_INC_M -> m  + 1
        C_DEC_M -> m  - 1
        C_ADD_D_M -> d + m 
        C_SUB_D_M -> d - m 
        C_SUB_M_D -> m  - d
        C_AND_D_M -> d .&. m 
        C_OR_D_M -> d .|. m
        -- HALT is a different beast ... this cannot be decoded in the output but must be handled beforehand ...
        C_HALT -> undefined

dest :: SymDest -> Int16 -> State CpuState (Int16, Bool)
dest d v = 
  case d of 
    D_NULL -> return (0, False)
    D_M -> return (v, True)
    D_D -> setD v >> return (0,False)
    D_MD -> setD v >> return (v, True)
    D_A -> setA v >> return (0,False)
    D_AM -> setA v >> return (v, True) 
    D_AD -> setA v >> setD v >> return (0, False) 
    D_AMD -> setA v >> setD v >> return (v, True)


jump :: SymJump -> Int16 -> Bool
jump j v =
  case j of
    J_NULL -> False
    J_JGT -> v >  0 
    J_JEQ -> v == 0
    J_JGE -> v >= 0
    J_JLT -> v <  0
    J_JNE -> v /= 0
    J_JLE -> v <= 0
    J_JMP -> True 