module Language where 

import Vector
import Logic
import Arithmetic
import LogicSeq

-- # Instruction
data BinInstr = 
      BinA BinInstrA
    | BinC BinInstrC
    deriving (Show, Eq)

data SymInstr = 
      SymA SymInstrA
    | SymC SymInstrC 
    deriving (Show, Eq)

bit16binInstr :: Bit16 -> BinInstr
bit16binInstr b 
    | isBit16InstrA b = BinA (bit16binInstrA b) 
    | otherwise = BinC (bit16binInstrC b)
    where 
        isBit16InstrA :: Bit16 ->  Bool
        isBit16InstrA (V16 O _ _ _ _ _ _ _ _ _ _ _ _ _ _ _) = True
        isBit16InstrA _ = False

binInstrSymInstr :: BinInstr -> SymInstr
binInstrSymInstr bi = case bi of 
    (BinA ba) -> SymA (binInstrAsymInstrA ba)
    (BinC bc) -> SymC (binInstrCsymInstrC bc)

bit16symInstr :: Bit16 -> SymInstr
bit16symInstr = binInstrSymInstr . bit16binInstr

type Label = String
type Addr = Int -- should be unsigned int
symInstrBit16 :: (Label -> Addr)
symInstrBit16 = undefined

-- # A Instruction
-- The A instruction is used to set the A register to a 5 bit value
-- @value where value is either a non-negative decimal number or a symbol referring to such number 
data BinInstrA = BinInstrA Bit15 deriving (Show, Read, Eq) 

data SymInstrA = 
      SymValue Int 
    | SymLabel String
    deriving (Show, Read, Eq)

bit16binInstrA :: Bit16 -> BinInstrA
bit16binInstrA (V16 O a15 a14 a13 a12 a11 a10 a09 a08 a07 a06 a05 a04 a03 a02 a01) =
    BinInstrA (V15 a15 a14 a13 a12 a11 a10 a09 a08 a07 a06 a05 a04 a03 a02 a01)

binInstrAsymInstrA :: BinInstrA -> SymInstrA
binInstrAsymInstrA (BinInstrA a) = SymValue (valueToUnsignedInt a)

valueToUnsignedInt :: Bit15 -> Int
valueToUnsignedInt (V15 a15 a14 a13 a12 a11 a10 a09 a08 a07 a06 a05 a04 a03 a02 a01) =
    int a15 * 2^14 +
    int a14 * 2^13 +
    int a13 * 2^12 +
    int a12 * 2^11+
    int a11 * 2^10 +
    int a10 * 2^9 + 
    int a09 * 2^8 + 
    int a08 * 2^7 + 
    int a07 * 2^6 + 
    int a06 * 2^5 + 
    int a05 * 2^4 + 
    int a04 * 2^3 + 
    int a03 * 2^2 + 
    int a02 * 2^1 + 
    int a01 * 2^0
    where 
        int I = 1
        int O = 0

-- non-total!
symInstrAbinInstrA :: (String -> Int) -> SymInstrA -> BinInstrA
symInstrAbinInstrA f (SymLabel label) = BinInstrA . unsignedIntToValue . f $ label
symInstrAbinInstrA _ (SymValue i) = BinInstrA (unsignedIntToValue i)

unsignedIntToValue :: Int -> Bit15
unsignedIntToValue i = 
    let rs = fmap intToBinary1 . take 15 $ (g i) 
        -- should be the last n elments of the list ...
        fill = replicate (15 - length rs) O
        [a15, a14, a13, a12, a11, a10, a09, a08, a07, a06, a05, a04, a03, a02, a01] = fill ++ rs
    in V15 a15 a14 a13 a12 a11 a10 a09 a08 a07 a06 a05 a04 a03 a02 a01
    where
        f :: Int -> (Int, Int)
        f i = (quot i 2, mod i 2) 

        g :: Int -> [Int]
        g i = case f i of
            (0, r) -> [r]
            (q, r) -> g q ++ [r]
        
        intToBinary1 :: Int -> Bit
        intToBinary1 0 = O
        intToBinary1 1 = I

-- # C Instruction
-- dest = comp; jump
data BinInstrC = BinInstrC Bit7 Bit3 Bit3 deriving (Show, Eq)

data SymInstrC = SymInstrC SymComp SymDest SymJump deriving (Show, Eq)

bit16binInstrC :: Bit16 -> BinInstrC 
bit16binInstrC (V16 I _ _ a c01 c02 c03 c04 c05 c06 d01 d02 d03 j01 j02 j03) =
    BinInstrC (binComp a c01 c02 c03 c04 c05 c06) (binDest d01 d02 d03) (binJump j01 j02 j03)

binInstrCbit16 :: BinInstrC -> Bit16
binInstrCbit16 (BinInstrC (V7 a c01 c02 c03 c04 c05 c06) (V3 d01 d02 d03) (V3 j01 j02 j03)) =
    V16 I I I a c01 c02 c03 c04 c05 c06 d01 d02 d03 j01 j02 j03

symInstrCbinInstrC :: SymInstrC -> BinInstrC
symInstrCbinInstrC (SymInstrC sc sd sj) = BinInstrC (symCompBinComp sc) (symDestBinDest sd) (symJumpBinJump sj)

binInstrCsymInstrC :: BinInstrC -> SymInstrC
binInstrCsymInstrC (BinInstrC bc bd bj) = SymInstrC (binCompSymComp bc) (binDestSymDest bd) (binJumpSymJump bj)

data SymComp =
      C_1
    | C_0
    | C_NEG_1
    | C_A
    | C_D
    | C_NOT_D
    | C_NOT_A
    | C_NEG_D
    | C_NEG_A
    | C_INC_D
    | C_INC_A
    | C_DEC_D
    | C_DEC_A
    | C_ADD_D_A
    | C_SUB_D_A
    | C_SUB_A_D
    | C_AND_D_A
    | C_OR_D_A
    | C_M           -- Memory[A] (value in memory addressed by A)
    | C_NOT_M
    | C_NEG_M
    | C_INC_M
    | C_DEC_M
    | C_ADD_D_M
    | C_SUB_D_M
    | C_SUB_M_D
    | C_AND_D_M
    | C_OR_D_M
    | C_RAW Bit7
    deriving (Show, Read, Eq)

data SymDest =
      D_NULL        -- the value is not stored anywhere
    | D_M           -- Memory[A] (memory addressed by A)
    | D_D
    | D_MD
    | D_A
    | D_AM
    | D_AD
    | D_AMD
    deriving (Show, Read, Eq) 

data SymJump =      
      J_NULL        -- no jump
    | J_JGT         -- if alu_out >  0 then jump 
    | J_JEQ         -- if alu_out =  0 then jump
    | J_JGE         -- if alu_out >= 0 then jump
    | J_JLT         -- if alu_out <  0 then jump
    | J_JNE         -- if alu_out /= 0 then jump
    | J_JLE         -- if alu_out <= 0 then jump
    | J_JMP         -- jump
    deriving (Show, Eq)

symCompBinCompLUT :: [(SymComp, Bit7)]
symCompBinCompLUT = 
    [ ( C_0         , binComp O I O I O I O )
    , ( C_1         , binComp O I I I I I I )
    , ( C_NEG_1     , binComp O I I I O I O )
    , ( C_D         , binComp O O O I I O O )
    , ( C_A         , binComp O I I O O O O )
    , ( C_NOT_D     , binComp O O O I I O I )
    , ( C_NOT_A     , binComp O I I O O O I )
    , ( C_NEG_D     , binComp O O O I I I I )
    , ( C_NEG_A     , binComp O I I O O I I )
    , ( C_INC_D     , binComp O O I I I I I )
    , ( C_INC_A     , binComp O I I O I I I )
    , ( C_DEC_D     , binComp O O O I I I I )
    , ( C_DEC_A     , binComp O I I O O I I )
    , ( C_ADD_D_A   , binComp O O O O O I O )
    , ( C_SUB_D_A   , binComp O O I O O I I )
    , ( C_SUB_A_D   , binComp O O O O I I I )
    , ( C_AND_D_A   , binComp O O O O O O O )
    , ( C_OR_D_A    , binComp O O I O I O I )
    , ( C_M         , binComp I I I O O O O )
    , ( C_NOT_M     , binComp I I I O O O I )
    , ( C_NEG_M     , binComp I I I O O I I )
    , ( C_INC_M     , binComp I I I O I I I )
    , ( C_DEC_M     , binComp I I I O O I O )
    , ( C_ADD_D_M   , binComp I O O O O I O )
    , ( C_SUB_D_M   , binComp I O I O O I I )
    , ( C_SUB_M_D   , binComp I O O O I I I )
    , ( C_AND_D_M   , binComp I O O O O O O )
    , ( C_OR_D_M    , binComp I O I O I O I )]

binComp :: Bit -> Bit -> Bit -> Bit -> Bit -> Bit -> Bit -> Bit7
binComp = V7

symCompBinComp :: SymComp -> Bit7
symCompBinComp = fst (bijective symCompBinCompLUT)

binCompSymComp :: Bit7 -> SymComp
binCompSymComp bc = case lookup bc (commuteLUT symCompBinCompLUT) of
    (Just sc) -> sc
    Nothing -> C_RAW bc

symDestBinDestLUT :: [(SymDest, Bit3)]
symDestBinDestLUT = 
    [ ( D_NULL  , binDest O O O )
    , ( D_M     , binDest O O I )
    , ( D_D     , binDest O I O )
    , ( D_MD    , binDest O I I )
    , ( D_A     , binDest I O O )
    , ( D_AM    , binDest I O I )
    , ( D_AD    , binDest I I O )
    , ( D_AMD   , binDest I I I )]

binDest :: Bit -> Bit -> Bit -> Bit3
binDest = V3

symDestBinDest :: SymDest -> Bit3
symDestBinDest = fst (bijective symDestBinDestLUT)

binDestSymDest :: Bit3 -> SymDest
binDestSymDest = snd (bijective symDestBinDestLUT)

symJumpBinJumpLUT :: [(SymJump, Bit3)]
symJumpBinJumpLUT =
    [ ( J_NULL  , binJump O O O )
    , ( J_JGT   , binJump O O I )
    , ( J_JEQ   , binJump O I O )
    , ( J_JGE   , binJump O I I )
    , ( J_JLT   , binJump I O O )
    , ( J_JNE   , binJump I O I )
    , ( J_JLE   , binJump I I O )
    , ( J_JMP   , binJump I I I )]

binJump :: Bit -> Bit -> Bit -> Bit3
binJump = V3

symJumpBinJump :: SymJump -> Bit3
symJumpBinJump = fst (bijective symJumpBinJumpLUT)

binJumpSymJump :: Bit3 -> SymJump
binJumpSymJump = snd (bijective symJumpBinJumpLUT)

-- helpers
bijective :: (Eq a, Eq b ) => [(a,b)] -> (a -> b, b -> a)
bijective abLUT = (lookupUnsafe abLUT, lookupUnsafe baLUT)
    where 
        baLUT = commuteLUT abLUT
        lookupUnsafe xys x = let (Just y) = lookup x xys in y 

commuteLUT :: [(a, b)] -> [(b, a)]
commuteLUT = fmap (\(a,b) -> (b,a))

-- ----------------------------------------------------------------------------
-- # predefined symbols

-- ## virtual registers

virtual_registers :: [(String, Bit15)]
virtual_registers = 
    [ ("R0",    (V15 O O O O O O O O O O O O O O O))
    , ("R1",    (V15 O O O O O O O O O O O O O O I))
    , ("R2",    (V15 O O O O O O O O O O O O O I O))
    , ("R3",    (V15 O O O O O O O O O O O O O I I))
    , ("R4",    (V15 O O O O O O O O O O O O I O O))
    , ("R5",    (V15 O O O O O O O O O O O O I O I))
    , ("R6",    (V15 O O O O O O O O O O O O I I O))
    , ("R7",    (V15 O O O O O O O O O O O O I I I))
    , ("R8",    (V15 O O O O O O O O O O O I O O O))
    , ("R9",    (V15 O O O O O O O O O O O I O O I))
    , ("R10",   (V15 O O O O O O O O O O O I O I O))
    , ("R11",   (V15 O O O O O O O O O O O I O I I))
    , ("R12",   (V15 O O O O O O O O O O O I I O O))
    , ("R13",   (V15 O O O O O O O O O O O I I O I))
    , ("R14",   (V15 O O O O O O O O O O O I I I O))
    , ("R15",   (V15 O O O O O O O O O O O I I O I))

-- # predefined pointers
predefined_pointers :: [(String, Bit15)]
predefined_pointers =
    [ ("SP",    (V15 O O O O O O O O O O O O O O O))
    , ("LCL",   (V15 O O O O O O O O O O O O O O I))
    , ("ARG",   (V15 O O O O O O O O O O O O O I O))
    , ("THIS",  (V15 O O O O O O O O O O O O O I I))
    , ("THAT",  (V15 O O O O O O O O O O O O I O O))
    ]

-- # IO pointers
-- (0x4000)
screen :: (String, Bit15)
screen = ("SCREEN", (V15 O O O O O O O O O O O O O O O))
-- (0x6000)
kbd :: (String, Bit15)
kbd = ("SCREEN", (V15 O O O O O O O O O O O O O O O))

-- label symbols
-- user defined symbols which serve to label destinations of goto commands
-- declared by pseudo command (Xxx)
-- this directive defines the symbol to refer to the instruction memory location
-- of the next command in the programm
-- a label can be defined only once and can be used anywhere in the assembly program
-- even before the line in which it is defined

-- variable symbols 
-- any user defined symbol Xxx appearing in an assembly programm
-- that is not predefined and is not defined elsewhere using the Xxx Command
-- is treated as a variable and is assigned a unique memory address 
-- by the assembler starting at RAM address 16 (0x0010)

-- ----------------------------------------------------------------------------
-- testdata
--
-- adds all numbers from 1 .. 100
--
--      @i
--      M = 1
--      @sum
--      M = 0
--  (LOOP)
--      @i
--      D = M
--      @100
--      D=D-A
--      @END
--      D;JGT
--      @i
--      D=M
--      @sum
--      M=D+M
--      @i
--      M=M+1
--      @loop
--      0;JMP
--  (END)
--      @END
--      0;JMP

set_a_label = SymA . SymLabel
set_a_value = SymA . SymValue
compute c d j = SymC (SymInstrC c d j) 

sum_1_to_100 = [
      set_a_label "i"
    , compute C_1 D_M J_NULL        -- i = 1
    , set_a_label "sum"
    , compute C_0 D_M J_NULL        -- sum = 0
    -- LOOP
    , set_a_label "i"
    , compute C_M D_D J_NULL        -- D = i
    , set_a_value 100               -- A = 100
    , compute C_SUB_D_A D_D J_NULL  -- D = i - 100
    , set_a_label "END"
    , compute C_D D_M J_JGT         -- if (i - 100) > 0 goto END
    , set_a_label "i"               
    , compute C_M D_D J_NULL        -- D = i
    , set_a_label "sum"
    , compute C_M D_A J_NULL        -- A = sum
    , compute C_ADD_D_A D_D J_NULL  -- D = i + sum
    , set_a_label "sum"
    , compute C_D D_M J_NULL
    , set_a_label "i"
    , compute C_M D_D J_NULL
    , compute C_INC_D D_M J_NULL
    , set_a_label "LOOP"
    , compute C_0 D_NULL J_JMP      -- goto loop
--    , END
    , set_a_label "END"
    , compute C_0 D_NULL J_JMP      -- goto end (recurse forever)
    ]