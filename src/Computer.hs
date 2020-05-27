module Computer where 

import Vector
import Logic
import Arithmetic
import LogicSeq
import Language

import Control.Monad.State
import Debug.Trace

import Text.Show.Pretty

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
data CpuIn = CpuIn {
      _inM :: Bit16,
      _instruction :: Bit16,
      _reset :: Bit
    } deriving (Show, Read, Eq)

data CpuState = CpuState {
      _A :: Bit16,
      _D :: Bit16,
      _PC :: Bit16
    } deriving (Show, Read, Eq)

data CpuOut = CpuOut {
      _outM :: Bit16,             -- value out
      _writeM :: Bit,             -- write value to memory 
      _addressM :: Bit15,         -- value memory address
      _programCounter :: Bit15,   -- address of next instruction
      _halt :: Bit                -- 20200505 jfm - introduced to stop execution
    } deriving (Show, Read, Eq)

-- cpu :: CpuIn -> State CpuState CpuOut 
-- cpu (CpuIn inM instruction reset) =
--     let outM = undefined
--         writeM = undefined 
--         addressM = undefined
--         pc = undefined
--     in return (CpuOut outM writeM addressM pc)

cpuControl :: Bit16 -> a
cpuControl instruction = undefined

-- -- symbolic cpu specification
-- cpuMock :: CpuIn -> State CpuState CpuOut
-- cpuMock (CpuIn inMem instr _) = do
--     (CpuState aReg dReg pcReg) <- get
--     let pcReg' = inc16 pcReg
--     put (CpuState aReg dReg pcReg')
--     let pcount = truncateBit16 pcReg' 
--         output outMem writeMem addrMem halt = return (CpuOut outMem writeMem addrMem pcount halt)  
--     let (V16 I _ _ a c01 c02 c03 c04 c05 c06 d01 d02 d03 j01 j02 j03) = instr
--         comp = getSymComp (V7 a c01 c02 c03 c04 c05 c06)
--         dest = getSymDest (V3 d01 d02 d03)
--         jump = getSymJump (V3 j01 j02 j03)
--         undefined
    
-- comp :: SymCopm -> State CpuState 
-- comp c = 
--     case comp of
--         C_1 -> undefined
--         C_0 -> undefined
--         C_NEG_1 -> undefined
--         C_A -> undefined
--         C_D -> undefined
--         C_NOT_D -> undefined
--         C_NOT_A -> undefined
--         C_NEG_D -> undefined
--         C_NEG_A -> undefined
--         C_INC_D -> undefined
--         C_INC_A -> undefined
--         C_DEC_D -> undefined
--         C_DEC_A -> undefined
--         C_ADD_D_A -> undefined
--         C_SUB_D_A -> undefined
--         C_SUB_A_D -> undefined
--         C_AND_D_A -> undefined
--         C_OR_D_A -> undefined
--         C_M -> undefined
--         C_NOT_M -> undefined
--         C_NEG_M -> undefined
--         C_INC_M -> undefined
--         C_DEC_M -> undefined
--         C_ADD_D_M -> undefined
--         C_SUB_D_M -> undefined
--         C_SUB_M_D -> undefined
--         C_AND_D_M -> undefined
--         C_OR_D_M -> undefined
--         C_HALT -> output inMem O (truncateBit16 aReg) I


    -- case dest of 
    --     D_NULL        -- the value is not stored anywhere
    --     D_M           -- Memory[A] (memory addressed by A)
    --     D_D
    --     D_MD
    --     D_A
    --     D_AM
    --     D_AD
    --     D_AMD
    -- case jump of
    --     J_NULL        -- no jump
    --     J_JGT         -- if alu_out >  0 then jump 
    --     J_JEQ         -- if alu_out =  0 then jump
    --     J_JGE         -- if alu_out >= 0 then jump
    --     J_JLT         -- if alu_out <  0 then jump
    --     J_JNE         -- if alu_out /= 0 then jump
    --     J_JLE         -- if alu_out <= 0 then jump
    --     J_JMP         -- jump
    -- where
    --     getSymComp :: Bit7 -> SymComp
    --     getSymComp = unsafeLookup (commuteLUT symCompBinCompLUT)
    --     getSymDest :: Bit3 -> SymDest
    --     getSymDest = unsafeLookup (commuteLUT symDestBinDestLUT)
    --     getSymJump :: Bit3 -> SymJump
    --     getSymJump = unsafeLookup (commuteLUT symDestBinDestLUT)

-- cpuIn :: CpuIn
-- cpuIn = CpuIn _O16 _O16 O    

-- cpuInHalt :: CpuIn
-- cpuInHalt = CpuIn _O16 _I16 O

-- initialCpuState :: CpuState 
-- initialCpuState = CpuState _O16 _O16 _O16

-- testCpu :: (CpuIn -> State CpuState CpuOut) -> CpuIn -> IO () 
-- testCpu cpu cpuIn = pPrint $ runState (cpuMock cpuIn) initialCpuState

-- runProgram :: (Bit15 -> Bit16) -> CpuState -- State ComputerState CpuState 
-- runProgram rom = fst $ runState (go _O15) initialComputerState 
--     where
--         go :: Bit15 -> State ComputerState CpuState
--         go iaddr = do
--             (ComputerState mem cpuS) <- get
--             -- need to get instruction here 
--             let ins = rom iaddr
--             -- either from cpu counter or we keep it elsewhere ...
--             -- or fixpoint for real?
--                 cpuOut@((CpuOut _ _ _ pc h), cpuS') = runState (cpuMock (CpuIn _O16 ins O)) cpuS
--             put (ComputerState mem cpuS')
--             case h of
--                 I -> return cpuS' -- we could also just return O and run to evaluate the state ...
--                 O -> trace (show cpuS') (go pc)

-- pHalt :: Bit15 -> Bit16
-- pHalt _ = _I16 

-- p1 :: Bit15 -> Bit16
-- p1 a = case lookup a rom of
--     Just b -> b
--     Nothing -> _I16 
--     where 
--         rom =
--             [ ((V15 O O O O O O O O O O O O O O O), _O16) 
--             , ((V15 O O O O O O O O O O O O O O I), _O16)
--             , ((V15 O O O O O O O O O O O O O I O), _O16)
--             , ((V15 O O O O O O O O O O O O O I I), _O16)
--             , ((V15 O O O O O O O O O O O O I O O), _O16)
--             , ((V15 O O O O O O O O O O O O I O I), _O16)
--             , ((V15 O O O O O O O O O O O O I I O), _I16)
--             ]


-- padBit15 :: Bit15 -> Bit16
-- padBit15 (V15 a15 a14 a13 a12 a11 a10 a09 a08 a07 a06 a05 a04 a03 a02 a01) = 
--     (V16 O a15 a14 a13 a12 a11 a10 a09 a08 a07 a06 a05 a04 a03 a02 a01)

-- truncateBit16 :: Bit16 -> Bit15
-- truncateBit16 (V16 O a15 a14 a13 a12 a11 a10 a09 a08 a07 a06 a05 a04 a03 a02 a01) =
--     (V15 a15 a14 a13 a12 a11 a10 a09 a08 a07 a06 a05 a04 a03 a02 a01)

-- data ComputerState = ComputerState {
--        _M :: KByte32
--     , _Cpu :: CpuState 
--     } deriving (Show, Read, Eq)


-- we need to supply a limit otherwise loops forever ...
-- or a halt command ...
-- computer :: (Bit15 -> Bit16) -> () -> State ComputerState ()
-- computer rom cs = do
--     (ComputerState _memS _cpuS) <- get
--     let (CpuState _ _ _pc) = _cpuS
--         ins = truncateBit16 _pc
--     undefined

-- forever?
-- ist doch schon alles da im cpu state ...s
-- u :: (Bit15, Bit16) -> State ComputerState (Bit15, Bit16)
-- u (pc, mem) = do
--     (ComputerState memState cpuState) <- get 
--     let ins = rom32K pc
--         ((CpuOut om wm am pc'), cpuState') = runState (cpu (CpuIn mem ins O)) cpuState
--         (mem', memState') = runState (ram32K om am wm) memState 
--     put (ComputerState memState' cpuState')
--     return (pc', mem')


-- test_u :: ((Bit15, Bit16), ComputerState)
-- test_u = runState (u (_O15, _O16)) initialComputerState

-- vermutlich funktioniert das so aber nicht ... 
-- läuft nicht mal auf undefined
-- kann aber auch sein, dass es an der lazy evaluation liegt
-- hätte aber gedacht, dass es nur eine iteration gibt
-- das ist quatsch, die addresse wird ja erst im nächsten schritt ausgegeben
-- ist also kein Fixpunkt in diretem sinn
-- u' :: State ComputerState (Bit15, Bit16)
-- u' = mfix u

-- cpuS :: CpuState 
-- cpuS = undefined

-- cpuI :: CpuIn
-- cpuI = undefined


-- -------------------------------------------------------------------

-- initialComputerState :: ComputerState
-- initialComputerState = ComputerState _OKByte32 initialCpuState



-- _O15 :: Bit15
-- _O15 = V15 O O O O O O O O O O O O O O O

-- _O16 :: Bit16
-- _O16 = stringToBit16 "OOOOOOOOOOOOOOOO"

-- _I16 :: Bit16
-- _I16 = stringToBit16 "IIIIIIIIIIIIIIII"

-- _OByte16 :: Byte16
-- _OByte16 = replicateV8 _O16

-- _OByte128 :: Byte128
-- _OByte128 = replicateV8 _OByte16

-- _OKByte1 :: KByte1
-- _OKByte1 = replicateV8 _OByte128

-- _OKByte8 :: KByte8
-- _OKByte8 = replicateV8 _OKByte1

-- _OKByte32 :: KByte32
-- _OKByte32 = replicateV8 _OKByte8