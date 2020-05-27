module Language where 

import Data.Maybe (catMaybes)
import Data.List (nub, (\\))
import Text.Pretty.Simple

import Data.Int(Int16)
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
type Addr = Int16-- should be unsigned int

symInstrBit16 :: [VarAddr] -> [LabelAddr] -> SymInstr -> Bit16
symInstrBit16 vas las (SymA si)  = 
    let unholyMadness = (unsafeLookup (vas ++ las)) in symInstrAbit16 unholyMadness si
symInstrBit16 _ _ (SymC sc) = symInstrCbit16 sc

-- # A Instruction
-- The A instruction is used to set the A register to a 5 bit value
-- @value where value is either a non-negative decimal number or a symbol referring to such number 
data BinInstrA = BinInstrA Bit15 deriving (Show, Read, Eq) 

data SymInstrA = 
      SymValue Int16
    | SymLabel String
    deriving (Show, Read, Eq)

bit16binInstrA :: Bit16 -> BinInstrA
bit16binInstrA (V16 O a15 a14 a13 a12 a11 a10 a09 a08 a07 a06 a05 a04 a03 a02 a01) =
    BinInstrA (V15 a15 a14 a13 a12 a11 a10 a09 a08 a07 a06 a05 a04 a03 a02 a01)

binInstrAsymInstrA :: BinInstrA -> SymInstrA
binInstrAsymInstrA (BinInstrA a) = SymValue (valueToUnsignedInt a)

valueToUnsignedInt :: Bit15 -> Int16
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
symInstrAbinInstrA :: (String -> Int16) -> SymInstrA -> BinInstrA
symInstrAbinInstrA f (SymLabel label) = BinInstrA . unsignedIntToValue . f $ label
symInstrAbinInstrA _ (SymValue i) = BinInstrA (unsignedIntToValue i)

unsignedIntToValue :: Int16-> Bit15
unsignedIntToValue i = 
    let rs = fmap intToBinary1 . take 15 $ (g i) 
        -- should be the last n elments of the list ...
        fill = replicate (15 - length rs) O
        [a15, a14, a13, a12, a11, a10, a09, a08, a07, a06, a05, a04, a03, a02, a01] = fill ++ rs
    in V15 a15 a14 a13 a12 a11 a10 a09 a08 a07 a06 a05 a04 a03 a02 a01
    where
        f :: Int16-> (Int16, Int16)
        f i = (quot i 2, mod i 2) 

        g :: Int16-> [Int16]
        g i = case f i of
            (0, r) -> [r]
            (q, r) -> g q ++ [r]
        
        intToBinary1 :: Int16-> Bit
        intToBinary1 0 = O
        intToBinary1 1 = I

binInstrAbit16 :: BinInstrA -> Bit16
binInstrAbit16 (BinInstrA (V15 a15 a14 a13 a12 a11 a10 a09 a08 a07 a06 a05 a04 a03 a02 a01)) =
    V16 O a15 a14 a13 a12 a11 a10 a09 a08 a07 a06 a05 a04 a03 a02 a01

symInstrAbit16 :: (String -> Int16) -> SymInstrA -> Bit16
symInstrAbit16 f = binInstrAbit16 . symInstrAbinInstrA f 

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

symInstrCbit16 :: SymInstrC -> Bit16 
symInstrCbit16 = binInstrCbit16 . symInstrCbinInstrC

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
    | C_HALT
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
    , ( C_OR_D_M    , binComp I O I O I O I )
    , ( C_HALT      , binComp I I I I I I I )    
    ]

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

-- ----------------------------------------------------------------------------
-- helpers
bijective :: (Eq a, Eq b ) => [(a,b)] -> (a -> b, b -> a)
bijective abLUT = (lookupUnsafe abLUT, lookupUnsafe baLUT)
    where 
        baLUT = commuteLUT abLUT
        lookupUnsafe xys x = let (Just y) = lookup x xys in y 

commuteLUT :: [(a, b)] -> [(b, a)]
commuteLUT = fmap (\(a,b) -> (b,a))


unsafeLookup :: Eq a => [(a, b)] -> a -> b
unsafeLookup m a = let (Just b) = lookup a m in b

-- ----------------------------------------------------------------------------
-- # predefined symbols

-- ## virtual registers
virtual_registers :: [(String, Int16)]
virtual_registers = 
    [ ("R0",    0)
    , ("R1",    1)
    , ("R2",    2)
    , ("R3",    3)
    , ("R4",    4)
    , ("R5",    5)
    , ("R6",    6)
    , ("R7",    7)
    , ("R8",    8)
    , ("R9",    0)
    , ("R10",   10)
    , ("R11",   11)
    , ("R12",   12)
    , ("R13",   13)
    , ("R14",   14)
    , ("R15",   15)
    ]

-- # predefined pointers
predefined_pointers :: [(String, Int16)]
predefined_pointers =
    [ ("SP",    1)
    , ("LCL",   2)
    , ("ARG",   3)
    , ("THIS",  4)
    , ("THAT",  5)
    ]

-- # IO pointers
-- (0x4000)
screen :: (String, Int16)
screen = ("SCREEN", 0)
-- (0x6000)
kbd :: (String, Int16)
kbd = ("SCREEN", 0)

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

data ASM = ASM (Maybe String) SymInstr
    deriving (Show, Eq)

type LabelAddr = (String, Int16)
type Var = String
type VarAddr = (String, Int16)

getLabelAddresses :: [ASM] -> [LabelAddr]
getLabelAddresses = go [] 0
    where 
        go ls _ [] = nub . reverse $ ls
        go ls i ((ASM (Just l) _) : as) = go ((l, i):ls) (i+1) as
        go ls i (_:as) = go ls (i+1) as

getLabels :: [ASM] -> [String]
getLabels = fmap fst . getLabelAddresses

getVariables :: [ASM] -> [Var]
getVariables as = let 
    labels = getLabels $ as
    names = nub . catMaybes . fmap getVariable $ as
    variables = names \\ labels
    in variables
    where 
        getVariable :: ASM -> Maybe Var
        getVariable (ASM _ (SymA (SymLabel v))) = Just v
        getVariable (_) = Nothing

getVariableAddresses :: [ASM] -> [VarAddr]
getVariableAddresses as = getVariables as `zip` [varAddrStart ..]
    where 
        varAddrStart = 16

x :: [ASM] -> ([VarAddr], [LabelAddr], [SymInstr])
x as = let
    vas = virtual_registers ++ predefined_pointers ++ getVariableAddresses as
    las = getLabelAddresses as
    sis = fmap getSymInstr as
    in (vas, las, sis)
    where 
        getSymInstr :: ASM -> SymInstr
        getSymInstr (ASM _ si) = si

y :: ([VarAddr], [LabelAddr], [SymInstr]) -> [Bit16]
y (vas, las, sis) = fmap (symInstrBit16 vas las) sis

assemble :: [ASM] -> [Bit16]
assemble = y . x

disassemble :: [Bit16] -> [ASM]
disassemble = fmap (ASM Nothing . bit16symInstr) 

renderASM :: [ASM] -> IO ()
renderASM = pPrint

renderBinary :: [ASM] -> IO ()
renderBinary = pPrint . assemble

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

_label l i = ASM (Just l) i 
asm = ASM Nothing

sum_1_to_100 :: [ASM]
sum_1_to_100 = [
      asm $ set_a_label "i"
    , asm $ compute C_1 D_M J_NULL        -- i = 1
    , asm $ set_a_label "sum"
    , asm $ compute C_0 D_M J_NULL        -- sum = 0
    , _label "LOOP" $ 
        set_a_label "i"
    , asm $ compute C_M D_D J_NULL        -- D = i
    , asm $ set_a_value 100               -- A = 100
    , asm $ compute C_SUB_D_A D_D J_NULL  -- D = i - 100
    , asm $ set_a_label "END"
    , asm $ compute C_D D_M J_JGT         -- if (i - 100) > 0 goto END
    , asm $ set_a_label "i"               
    , asm $ compute C_M D_D J_NULL        -- D = i
    , asm $ set_a_label "sum"
    , asm $ compute C_M D_A J_NULL        -- A = sum
    , asm $ compute C_ADD_D_A D_D J_NULL  -- D = i + sum
    , asm $ set_a_label "sum"
    , asm $ compute C_D D_M J_NULL
    , asm $ set_a_label "i"
    , asm $ compute C_M D_D J_NULL
    , asm $ compute C_INC_D D_M J_NULL
    , asm $ set_a_label "LOOP"
    , asm $ compute C_0 D_NULL J_JMP      -- goto loop
    , _label "END" $ 
        set_a_label "END"
    , asm $ compute C_0 D_NULL J_JMP      -- goto end (recurse forever)
    ]