import GHC.Int
import Data.Char

{-
Reference C implementation: https://www.ahl27.com/posts/2023/01/6502-emu1/
Opcode table: https://llx.com/Neil/a2/opcodes.html
Opcode details: https://www.atariarchives.org/alp/appendix_1.php
65c02 opcode details: https://mixinc.net/atari/mac65.htm
Decimal mode: https://www.atarimagazines.com/compute/issue51/222_1_MACHINE_LANGUAGE.php
Decimal mode: http://www.6502.org/tutorials/decimal_mode.html
Atari FAQ: https://www.ataricompendium.com/faq/faq.html
TIA FAQ: https://www.ataricompendium.com/faq/vcs_tia/vcs_tia.html
6502 assembly: https://en.wikibooks.org/wiki/6502_Assembly

The Atari 2600 has the following components:
 - MPU 6507 chip, a low-cost version of 6502
 - Television Interface Adapter (TIA) chip
 - RIOT chip: 6532 RAM, I/O, and timer

Specs:
 - RAM: 128 in VLSI? might be mapped
 - ROM: 4K maximum
 - TIA: 3.579545 MHz (NTSC frequency, 315/88 * 1e6)
 - MPU clock: 1.19 MHz (1/3 TIA, so 105/88 * 1e6 = 1193181.8 Hz)
 - Graphics clock: 1.19 MHz

The 6507 chip differs from 6502:
 - 6502 has 64k memory (16 bits) vs 6507's 4k (12 bits)

Need to focus on making this *declarative*. What functionality do we want?
(in order of importance):
 - Step execution
 - Disassemble
 - Assemble

What, fundamentally, are the properties we need to model?
 - Step execution:
   - Registers
   - Flags
   - Memory
   - Cycles to execute the operation
   - Cycles to fetch the location 
   - Translate opcode + operand bytes -> operation + address
   - Processor mutation of the operation
   - A wrapper to fetch instructions and increment PC
 - Disassemble:
   - Operation -> opname
   - Translate opcode + operand bytes -> operation + location
 - Assemble:
   - Opname -> operation
   - Translate operation + location -> opcode + operand bytes
   - Available address modes (to reject if invalid)

If we explicitly model decoding, we get both step execution and disassembly
 for free provided we implement location disassembly:
 dis op = (name op) ++ " " ++ (dis $ location op)
Though it's a little complicated if we want labels, I don't think that matters.

In the reverse case, assembly requires operators to know how they're assembled
 given a location, so that must be part of their definition.

If we want a bijection between opnames and operations without duplication (to
 get a single source of truth), the names and operations must be separated and
 then combined by a wrapper around the injection. It's actually rather nice
 that it works out like that, because it means we can ignore the name completely
 during execution as it's not included with the operation itself.

Terminology used:
 - opcode - the literal byte encoding an operation
 - opname - name of an operation
 - apply - a single state mutation
 - execute - full fetch, decode, apply cycle
 - immediate - data which comes after an opcode

Assembly syntax:
 - # = Immediate literal value
 - d = "Direct" 8-bit zero-page address immediate
 - a = "Absolute" 16-bit address immediate
 - A/X/Y = Registers
 - , = Add two locations
 - (...) = dereference
 - So `inc (d,X)`:
   - Calculates 8-bit zero-page address `addr = d + X`
   - Fetches the 16-bit value at `addr`
   - Interprets that as an absolute address
   - Increments the byte at that address
 -}
 
{-- Investigating a better model --}

data BusState = BusPass | BusRead Word16 | BusWrite Word8

class BusModule a where
  modStep :: a -> BusState -> State a BusState

data Atari2600 = Atari2600 {
  busModules :: [BusModule]
}

{-----------------------------------}

-- Convert a BCD integer to binary
dec2bin :: Int -> Int
dec2bin dec = 10*d1 + d0
  where d0, d1 = dec `divMod` 16

-- Convert a binary integer to BCD
bin2dec :: Int -> Int
bin2dec bin = 16*d1 + d0
  where d0, d1 = ((bin + if bin < 0 then 100 else 0) `mod` 100) `divMod` 10

-- Widen Int8 to [0, 255] Int
uint8 :: Integral a => a -> Int
uint8 i = (fromIntegral i :: Int) .&. 0xFF

-- Build a word from two bytes in big endian order
word :: Int8 -> Int8 -> Int16
word hi lo = (hi `shiftL` 8) .|. lo

-- Create a mask of n lower bits
mask :: (Bits a, Integral a) => Int -> a
mask n = bit n - 1

-- Terminology here differs from lists because bits only have a given size in some contexts
-- The key is (take, drop) should work with big-endian intuition

-- Return everything other than the least significant n bits
takeBits :: (Bits a, Integral a) => Int -> a -> a
takeBits n int = int `shiftR` n

-- Return the least significant n bits
dropBits :: (Bits a, Integral a) => Int -> a -> a
dropBits n int = int .&. mask n

-- Return a tuple of the bits before and after the least significant n bits
splitBits :: (Bits a, Integral a) => Int -> a -> (a, a)
splitBits n int = takeBits n int, dropBits n int

-- Processor state record
data Processor = Processor {
  pc     ::  Int16, -- Program Counter
  rP     ::  Int8,  -- Processor status flags
  rS     ::  Int8,  -- Stack pointer
  rA     ::  Int8,  -- Accumulator
  rX     ::  Int8,
  rY     ::  Int8,
  memory :: [Int8],
  cycles ::  Int    -- # cycles since boot
}

type PState = State Processor

readByte :: Int16 -> PState Int8
readByte addr = do
  mem <- gets memory
  addCycles 1
  return $ mem!!addr

-- Atari is little-endian for 16-bit reads, no alignment constraints
readWord :: Int16 -> PState Int16
readWord addr = do
  mem <- gets memory
  lo  <- readByte $ addr
  hi  <- readByte $ addr + 1
  return $ word hi lo

writeByte :: Int16 -> Int8 -> PState ()
writeByte addr value = do
  mem <- gets memory
  put $ state { memory = (take addr mem) ++ (value:(drop (addr+1) mem)) }
  addCycles 1

writeWord :: Int16 -> Int16 -> PState ()
writeWord addr value = do
  let (hi, lo) = splitBits 8 value
  writeByte (addr  ) lo
  writeByte (addr+1) hi

-- Add cycles and pass through a value
addCycles :: Int -> PState ()
addCycles c = modify $ \p -> p { cycles = (cycles state) + c }

-- Apply a function to the PC and update it
modifyPC :: (Int16 -> Int16) -> PState ()
modifyPC f = do
  old <- gets pc
  modify $ \s -> s { pc = f old }

-- Abstract location, may take cycles to get
class Location a where
  getLoc :: a -> PState Int
  setLoc :: a -> Int -> PState ()

-- Register enumeration
data Register = P | S | A | X | Y
  deriving (Show, Enum)

instance Location Register where
  getLoc P = rP
  getLoc S = rS
  getLoc A = rA
  getLoc X = rX
  getLoc Y = rY
  
  setLoc P value state = state { rP = value }
  setLoc S value state = state { rS = value }
  setLoc A value state = state { rA = value }
  setLoc X value state = state { rX = value }
  setLoc Y value state = state { rY = value }

-- Processor Status flag enumeration
data Flag = C | Z | I | D | B | V | N
  deriving (Eq, Show)
instance Enum Flag where
  fromEnum flag = case flag of
    C -> 0 -- Carry
    Z -> 1 -- Zero
    I -> 2 -- Interrupt
    D -> 3 -- Decimal
    B -> 4 -- Break
    -- Bit 5 unused
    V -> 6 -- oVerflow
    N -> 7 -- Negative
  
  toEnum flag = [C, Z, I, D, B, undefined, V, N]!!flag

-- Apply a function to the flags
applyFlag :: (Int8 -> Int -> a) -> Flag -> Processor -> a
applyFlag f flag p = rP p `f` fromEnum flag

-- Modify the flag using a given function
modifyFlag :: (Int8 -> Int -> Int8) -> Flag -> PState ()
modifyFlag f flag = modify $ \p -> p { rP = applyFlag f flag p }

clearFlag = modifyFlag clearBit
setFlag   = modifyFlag   setBit
testFlag  = applyFlag   testBit
assignFlag flag b = (if b then setFlag else clearFlag) flag

-- Subclass of Location which can be defined by a single address
class Location a => Address a where
  address :: a -> Int16 -> PState Int16
  
  get addr = do
    addr' <- gets $ address addr
    readByte  addr'
  set addr = do
    addr' <- gets $ address addr
    writeByte addr'

{- Address modes -}

-- Operand implied by the instruction (so no extra operand bytes)
data Implied = Implied

-- An 8-bit immediate
data Immediate = Immediate Int8   -- #
instance Location

data AddrZeroX' = AddrZeroX' Int8 -- (d,X)
instance Address AddrZeroX' where
  address (AddrZeroX' addr) = do
    x <- gets rX
    readWord $ addr + x

data AddrZero = AddrZero Int8     -- d
instance Address AddrZero where
  address (AddrZero addr) = do
    return addr

data AddrAbs = AddrAbs Int16      -- a
instance Address AddrAbs where
  address (AddrAbs addr) = do
    return addr

data AddrZero'Y = AddrZero'Y Int8 -- (d),Y
instance Address AddrZero'Y where
  address (AddrZero'Y addr) = do
    addr' <- readWord addr
    y <- gets rY
    return $ addr' + y

data AddrZeroX = AddrZeroX Int8   -- d,X
instance Address AddrZeroX where
  address (AddrZeroX addr) = do
    x <- gets rX
    return $ addr + x

data AddrAbsY = AddrAbsY Int16    -- a,Y
instance Address AddrAbsY where
  address (AddrAbsY addr) = do
    y <- gets rY
    return $ addr + y

data AddrAbsX = AddrAbsX Int16    -- a,X
instance Address AddrAbsX where
  address (AddrAbsX addr) = do
    x <- gets rX
    return $ addr + x

{- Operation type classes -}

-- Abstract interface for an operation
class Op a where
  -- Apply the operation to the state
  apply  :: a -> Operand -> PState ()
  -- Disassemble the operation
  dis    :: a -> String -> Operand -> Maybe String
  -- Assemble the operation (Nil if invalid operand)
  asm    :: a -> Operand -> Maybe [Int8]
  
  dis self name ins = name ++ " " ++ show $ operand ins

-- Opcodes only available on the 65c02
data Op65c02 = Op65c02 Op
instance Op Op65c02 where
  apply (Op65c02  op) = apply op
  dis   (Op65c02  op) = dis   op
  asm   (Op65c02  op) = asm   op

-- Opcodes only available on the 65c816
data Op65c816 = Op65c816 Op
instance Op Op65c02 where
  apply (Op65c816 op) = apply op
  dis   (Op65c816 op) = dis   op
  asm   (Op65c816 op) = asm   op

-- An operation with its operand
data Instruction = Instruction {
  operation :: Op,
  operand   :: Operand
}

-- Operators which are simple applications of a function
-- Handles N and Z flags, cycles
class Op a => ApplyOp a where
  applyOp :: a -> Operand -> PState ()
  
  apply op oper = do
    result <- applyOp op
    assignFlag Z (new == 0)
    assignFlag N (new  < 0)
    addCycles (cycles op)

-- Operations of the form L = f(L), updates N and Z but not C
class ApplyOp a => UnaryOp a where
  cycles :: a -> Int
  unaryApply :: a -> Int8 -> Int8
  
  applyOp op oper = do
    old <- getLoc oper
    let result = unaryApply op old
    setLoc oper result
    return result

-- A subclass of binary operations with an explicit destination
class ApplyOp a => BinaryOp a where
  binaryApply :: a -> Int8 -> Int8 -> Int8
  dst :: a -> Register
  dst op = A
  
  applyOp op oper = do
    d   <- dst op
    lhs <- getLoc d
    rhs <- getLoc oper
    let result = binaryApply op lhs rhs
    setLoc d result
    return result

-- Op which just updates a flag
class Op a => FlagOp a where
  applyFlag :: a -> (Flag, Bool)
  
  apply op oper = do
    let f, b = applyFlag op
    setFlag f b

-- Operations which actually use the carry flag, ADC and SBC
-- Also the only ones which respect decimal mode
class Op a => CarryOp a where
  carryApply :: a -> Int -> Int -> Int
  carry :: a -> Bool -> Bool
  
  apply op oper = do
    d   <- gets $ testFlag D
    c   <- gets $ testFlag C
    lhs <- gets rA
    rhs <- getLoc oper
    let
      f  = foldl $ carryApply op
      il = fromIntegral lhs :: Int
      ir = fromIntegral rhs :: Int
      ic = fromEnum c
    let raw, result, carry =
      if d then do -- Decimal mode
        -- Section 3.2.2 of decimal mode:
        -- 99 + 1 = 00 (but Z=0) suggesting Z is only set when the result is
        --  intentionally 0, eg 00 + 0 or 01 - 1, not when it overflows. 
        --  So, return the untruncated result
        let result = f (dec2bin il) [dec2bin ir, carry op ic]
        return (result, bin2dec result, (result .&. 0x3FF) `div` 100 > 0)
      else do      -- Binary mode
        let result = f il [ir, carry op ic]
        return (result, result, result `testBit` 8)
    setLoc A result
    assignFlag C carry
    assignFlag Z (raw == 0)
    assignFLag V $ (lhs `xor` result) .&. 0x80 == 0
    assignFlag N (raw  < 0)
    
    addCycles 1

-- Branch on a flag
class Op a => BranchOp a where
  applyBranch :: a -> (Flag, Bool)
  
  apply op oper = do
    off <- getLoc oper
    let f, b = applyBranch op
    fv  <- testFlag f
    when (f == b) do
      modifyPC (+ off)
      -- +1 cycles when jumping?
    -- TODO: Cycles (complex because +1 when crossing page boundary?)

-- Operations on the stack
class Op => StackOp a where
  register :: a -> Register
  applyStack :: a -> Int -> PState ()
  
  apply op oper = do
    reg <- gets $ getLoc (register op)
    applyStack op reg
    -- Stack op covers the cycles

-- Operation which transfers between two locations
class Op a => TransferOp a where
  applyTransfer :: a -> Operand -> (Location, Location)
  
  apply op oper = do
    let src, dst = applyTransfer op oper
    v <- getLoc src
    setLoc dst v
    addCycles 1

class Op a => CompareOp a where
  applyCompare :: a -> Operand -> (Location, Location)
  
  apply op oper = do
    let x, y = applyCompare op oper
    lhs <- getLoc x
    rhs <- getLoc y
    let result = lhs - rhs
    assignFlag Z (result == 0)
    assignFlag N (result  < 0)

class Show a => Location a where
  -- How many cycles it takes to access a location
  get    :: a -> PState Int8
  set    :: a -> PState ()

-- Push to the stack (SP)
push :: Int8 -> Processor -> Processor
push value state = (writeMemory sp value) { rS = sp - 1 }
  where sp = rS state

-- Pull from the stack (SP) - this is the terminology used in the atari's
--  documentation, as opposed to pop.
pull :: Processor -> (Int8, Processor)
pull state = readWord (sp state), state { sp = sp + 1 }

{----------------}
{-- Operations --}
{----------------}

data NOP = NOP -- No OPeration
instance Op NOP where
  apply op oper = do
    addCycles 1

{- Arithmetic -}

data INC = INC -- INCrement
instance UnaryOp INC where
  cycles = const 0
  unaryApply op = (+1)
  asm op oper = case oper of
    AddrZero a -> Just 0xE6:[0xE6] -- inc d
    AddrZeroX -> Just [0xF6] -- inc d,X
    AddrAbs -> Just [0xEE] -- inc a
    AddrAbsX -> Just [0xFE] -- inc a,X
    LocReg reg -> case reg of
      A -> Just [0x1A] -- inc A
      X -> Just [0xE8] -- inx
      Y -> Just [0xC8] -- iny
      _ -> Nil
    _ -> Nil

data DEC = DEC -- DECrement
instance UnaryOp DEC where
  cycles = const 0
  unaryApply op = (-1)
  asm op oper = case oper of
    AddrZero -> Just [0xC6] -- dec d
    AddrZeroX -> Just [0xD6] -- dec d,X
    AddrAbs -> Just [0xCE] -- dec a
    AddrAbsX -> Just [0xDE] -- dec a,X
    LocReg reg -> case reg of
      A -> Just [0x3A] -- dec A
      X -> Just [0xCA] -- dex
      Y -> Just [0x88] -- dey
      _ -> Nil
    _ -> Nil

data ORA = ORA -- OR Accumulator
instance BinaryOp ORA where
  binaryApply = const (.|.)

data AND = AND -- AND
instance BinaryOp AND where
  binaryApply = const (.&.)

data EOR = EOR -- Exclusive OR
instance BinaryOp EOR where
  binaryApply = const (xor)
  
data ADC = ADC -- AdD with Carry
instance CarryOp ADC where
  carryApply op l r c = l + r + c
  decCarry   op = (>   99)
  binCarry   op = (> 0xFF)

data SBC = SBC -- SuBtract with Carry
instance CarryOp SBC where
  -- Carry meaning is inverted for SBC
  carryApply op l r c = l - r - (1 - c)
  decCarry   op = (< 0)
  binCarry   op = (`testBit` 9)

{- Shift operations -}

data ASL = ASL -- Arithmetic Shift Left
instance UnaryOp ASL where
  cycles = const 1
  unaryApply op = (`shiftL` 1)
data LSR = LSR -- Logical Shift Right
instance UnaryOp LSR where
  cycles = const 1
  unaryApply op = (`shiftR` 1)

data ROL = ROL -- ROtate Left
instance UnaryOp ROL where
  cycles = const 1
  unaryApply op = (`rotateL` 1)
data ROR = ROR -- ROtate Right
instance UnaryOp ROR where
  cycles = const 1
  unaryApply op = (`rotateR` 1)

{- Flag operations -}

data CMP = CMP -- CoMPare accumulator
instance CompareOp CMP where
  applyCompare op oper = A, oper

data CPX = CPX -- ComPare X
instance CompareOp CPX where
  applyCompare op oper = X, oper
data CPY = CPY -- ComPare Y
instance CompareOp CPY where
  applyCompare op oper = Y, oper

data BIT = BIT = -- BIT test
instance Op BIT where
  apply op oper = do
    a <- gets rA
    v <- getLoc oper
    let result = a .&. v
    -- Not sure why this is useful but docs say BIT sets C to bit 6
    assignFlag C (result `testBit` 6)
    assignFlag Z (result == 0)
    assignFlag N (result  < 0)
    -- Seems to take 0 cycles inherently, with all cycles handled by
    --  the data it moves around.

data CLC = CLC -- CLear Carry
instance FlagOp CLC where
  applyFlag op = C, False
data SEC = SEC -- SEt Carry
instance FlagOp SEC where
  applyFlag op = C, True

data CLI = CLI -- CLear Interrupt
instance FlagOp CLI where
  applyFlag op = I, False
data SEI = SEI -- SEt Interrupt
instance FlagOp SEI where
  applyFlag op = I, True

data CLV = CLV -- CLear oVerflow
instance FlagOp CLV where
  applyFlag op = V, False
-- No SEV

data CLD = CLD -- CLear Decimal
instance FlagOp CLD where
  applyFlag op = D, False
data SED = SED -- SEt Decimal
instance FlagOp SED where
  applyFlag op = D, True

data BRK = BRK -- BReaK
instance FlagOp BRK where
  applyFlag op = B, True
-- No clear B, think it's cleared by the debugger?

{- Branch operations -}

data JMP = JMP -- JuMP
instance Op JMP where
  apply op oper = do
    new <- getLoc oper
    setPC new
    addCycles 2 -- ?

data JSR = JSR -- Jump to SubRoutine
instance Op JSR where
  apply op oper = do
    old <- gets pc
    pushWord $ old
    new <- getLoc oper
    setPC new
    addCycles 4 -- TODO: +1 for page boundary?

data RTS = RTS -- ReTurn from Subroutine
instance Op RTS where
  apply op oper = do
    old <- pullWord
    setPC old
    addCycles 4 -- TODO: +1 for page boundary?

data INT = INT -- INTerrupt, pseudo-instruction
instance Op INT where
  apply op oper = do
    apply JSR oper
    flags <- gets rP
    pushByte flags

data RTI = RTI -- ReTurn from Interrupt
instance Op RTI where
  apply op oper = do
    apply RTS oper
    flags <- pullByte
    modify $ \p -> p { rP = flags }

data BPL = BPL -- Branch on PLus
instance BranchOp BPL where
  applyBranch op = N, False
data BMI = BMI -- Branch on MInus
instance BranchOp BMI where
  applyBranch op = N, True

data BVC = BVC -- Branch on oVerflow Clear
instance BranchOp BVC where
  applyBranch op = V, False
data BVS = BVS -- Branch on oVerflow Set
instance BranchOp BVS where
  applyBranch op = V, True

data BCC = BCC -- Branch on Carry Clear
instance BranchOp BCC where
  applyBranch op = C, False
data BCS = BCS -- Branch on Carry Set
instance BranchOp BCS where
  applyBranch op = C, True

data BNE = BNE -- Branch on Not Equal to zero
instance BranchOp BNE where
  applyBranch op = Z, False
data BEQ = BEQ -- Branch on EQual to zero
instance BranchOp BEQ where
  applyBranch op = Z, True

{- Stack operations -}

data PHP = PHP -- PusH Processor status flags
instance StackOp PHP where
  register op = P
  applyStack op = pushByte
data PLP = PLP -- PulL Processor status flags
instance StackOp PLP where
  register op = P
  applyStack op = pullByte

data PHA = PHA -- PusH Accumulator
instance StackOp PHA where
  register op = A
  applyStack op = pushByte
data PLA = PLA -- PulL Accumulator
instance StackOp PLA where
  register op = A
  applyStack op reg = do
    a <- pullByte
    assignFlag N (a  < 0)
    assignFlag Z (a == 0)
    return a

-- PHX, PLX, PHY, PLY are 65c02 instructions

{- Transfer operations -}

data STA = STA -- STore Accumulator into memory
instance TransferOp STA where
  applyTransfer op oper = A, oper
data LDA = LDA -- LoaD Accumulator from memory
instance TransferOp LDA where
  applyTransfer op oper = oper, A

data STX = STX -- STore X into memory
instance TransferOp STX where
  applyTransfer op oper = X, oper
data LDX = LDX -- LoaD X from memory
instance TransferOp LDX where
  applyTransfer op oper = oper, X

data STY = STY -- STore Y into memory
instance TransferOp STY where
  applyTransfer op oper = Y, oper
data LDY = LDY -- LoaD Y from memory
instance TransferOp LDY where
  applyTransfer op oper = oper, Y

data TXA = TXA -- Transfer X to Accumulator
instance TransferOp TXA where
  applyTransfer op oper = X, A
data TAX = TAX -- Transfer Accumulator to X
instance TransferOp TAX where
  applyTransfer op oper = A, X

data TXS = TXS -- Transfer X to Stack pointer
instance TransferOp TXS where
  applyTransfer op oper = X, S
data TSX = TSX -- Transfer Stack pointer to X
instance TransferOp TSX where
  applyTransfer op oper = S, X

data TYA = TYA -- Transfer Y to Accumulator
instance TransferOp TYA where
  applyTransfer op oper = Y, A
data TAY = TAY -- Transfer Accumulator to Y
instance TransferOp TAY where
  applyTransfer op oper = A, Y

data DecodeError = Op65c02 | Op65c816
  deriving (Eq, Show)

data Result a = Ok a | DecodeError
  deriving (Eq, Show)

decodeGroup1 aaa = [
  ORA, AND,
  EOR, ADC,
  STA, LDA,
  CMP, SBC]!!aaa

decodeGroup2 aaa = [
  ASL, ROL,
  LSL, ROR,
  STX, LDX,
  DEC, INC]!!aaa

{-
At first I tried to follow the reference implementation for decoding, just
 returning the records instead of directly executing. However, the logic was
 complex and supported a weird subset of 65c02 opcodes which the 6507 doesn't.
 I want to be able to at least detect the unsupported opcodes, but don't need
 to model them directly.

Now, my strategy is to decode operations and operands separately. This way,
 I can separate the two each with checks for unsupported ops and consider
 an intersection of support. If either fails, we know it's unsupported. If
 both succeed, the pair must be the correct decoding.

The overarching pattern in the address modes can be split into 2 sets of 4
 "blocks" of columns (low nibbles) in the decoding table.
 - Block 0: Column 0, only 1 mode
 - Block 1: Column 1, 2 modes alternating
 - Block 2: Columns 3-4, 2 modes alternating
 - Block 3: Columns 5-7, 2 modes alternating

x|0|1|2|3|4|5|6|7
0 0 1 3-3 5-5-5-5  ar=0
1 0 2 4-4 6-6-6-6  ar=1 -> repeats for 8:F
2 0 1 3-3 5-5-5-5  ar=0
3 0 2 4-4 6-6-6-6  ar=1
...    <- lz (3:0)

Then the pattern repeats itself for the second half of the table with
 different modes. Exceptions are common, but this pattern is very regular.
 This kind of pattern fits with the leading 0 count, exponential. I feel
 like there's a clever way to index this, but it would be much easier to
 just build a 2x(1+3)x2 lookup table.
 -}

decodeOperand op
  -- Exceptions
  | op == 0x20 = Ok AddrAbs      -- JSR a
  | op == 0x6C = Ok AddrIndirect -- JMP (a)
  | op == 0xA2 = Ok Immediate    -- LDX #
  | op == 0xBE = Ok AddrAbsY     -- LDX a,Y
  | is_misc    = Ok Implied
  | is_sb2     = Ok Implied
  | lz == 3    = Ok $ [Immediate, Implied]!!t
  | otherwise  = [[
      [Ok AddrZero, Ok AddrZeroX],
      [Op65c816, Op65c02],
      [Ok AddrZeroX', Ok AddrZero'Y],
    ], [
      [Ok AddrAbs, Ok AddrAbsX],
      [Ok A, Op65c02],
      [Ok Immediate, Ok AddrAbsY]
    ]]!!t!!lz!!ar
  where
    is_misc = op .&. 0x9F == 0x00 -- 0mm00000
    is_sb2  = op .&. 0x8F == 0x8A -- 1xxx1010
    t  = fromEnum $ op `testBit` 3
    ar = fromEnum $ op `testBit` 4
    -- clz of least significant 3 bits
    lz = countLeadingZeros $ fromIntegral v :: Int8
      where v = shiftL op 5 .|. bit 4

-- Decode the bytecode into an operation and operand
decodeOp :: Int8 -> Int8 -> Int8 -> DecodeResult (Op, Operand)
decodeOp op
  -- Invalid ops up front to simplify logic
  | op `elem` [
    0x04, 0x0C,       -- TSB d   : a
    0x14, 0x1C,       -- TRB d   : a
    0x64, 0x9C,       -- STZ d   : a
    0x74, 0x9E,       -- STZ d,X : a,X
    0x80, 0x89        -- BRA r : BIT #
    0xDA, 0xFA,       -- PHX, PLX
  ] = Op65c02
  -- Single-byte group 1 (hhhh1000)
  | is_sb1 = Ok ([
    PHP, CLC, PLP, SEC,
    PHA, CLI, PLA, SEI,
    DEY, TYA, TAY, CLV,
    INY, CLD, INX, SED]!!hhhh, Implied)
  -- Single-byte group 2 (1xxx1010)
  | is_sb2 = Ok ([
      TXA, TXS,
      TAX, TSX,
      DEX, undefined,
      NOP, undefined]!!xxx, Implied)
  -- Misc instructions
  | is_misc = Ok case mm of
    0 -> BRK, Implied
    1 -> JSR, AddrAbs $ word hi lo
    2 -> RTI, Implied
    3 -> RTS, Implied
  | otherwise = case cc of
    1 -> decodeGroup1 aaa, addr_group1
    2 -> (case bbb of
      4 -> decodeGroup1 aaa
      7 -> decodeGroup2 aaa), addr_group2
    3 -> Op65c816
    0 -> case () of ()
      | bbb == 4  = [
        BPL, BMI,
        BVC, BVS,
        BCC, BCS,
        BNE, BEQ]!!(hhhh `shiftR` 1)
      | bbb == 0 && not (aaa `testBit` 2) = [
        BRK, JSR, RTI, RTS]!!mm, Implied
      | aaa==0,2 : -- 000???00
      | otherwise = decodeGroup3 aaa
    2 -> [ -- Group 1
      ORA, AND,
      EOR, ADC,
      STA, LDA,
      CMP, SBC]!!aaa, addr_group1
    1 -> [ -- Group 2
      ASL, ROL,
      LSL, ROR,
      STX, LDX,
      DEC, INC]
    [
    , [ 
      ASL, ROL,
      LSL, ROR,
      STX, LDX,
      DEC, INC
    ], [ -- Group 3
      case0,
      BIT, case2,
      JMP,
      STY, LDY,
      CPY, CPX
    ]
  ]!!cc!!aaa
  where
    is_sb1  =        llll == 0x08 -- hhhh1000
    is_sb2  = op .&. 0x8F == 0x8A -- 1xxx1010
    is_misc = op .&. 0x9F == 0x00 -- 0mm00000
    mm          = aaa -- Same thing if is_misc
    xxx         = hhhh .&. mask 3
    hhhh,  llll = splitBits 4
    -- G1:3 are of the form aaa bbb cc
    aaabbbcc    = op
    aaabbb,  cc = splitBits 2 aaabbbcc
    aaa   , bbb = splitBits 3 aaabbb
    addr_group1 = decode_group1_addr  bbb rest state
    addr_group2 = decode_group23_addr bbb rest state
    
    abs = word hi lo
    
    addr_group1  = case bbb of
      0 -> AddrZeroX' lo
      1 -> AddrZero   lo
      2 -> Immediate  lo
      3 -> AddrAbs    abs
      4 -> AddrZero'Y lo
      5 -> AddrZeroX  lo
      6 -> AddrAbsY   abs
      7 -> AddrAbsX   abs
    
    addr_group23 = case bbb of -- yyy1 0001 | yyy0 1101
      0 -> Immediate  lo
      1 -> AddrZero   lo
      2 -> A
      3 -> AddrAbs    abs
      4 -> 

    case 4:       // zero page indirect
      address = read_address(read_pc());
      break;
    
    case 5:       // zero page, X
      address = read_pc();
      // need a special check here for LDX/STX
      address += (highbits&6) == 4 ? y : x;
      address &= 0xFF;
      break;

    // No case 6 exists

    case 7:       // absolute, X
      address = read_address(pc);
      address += (highbits&6) == 4 ? y : x;
      break;
  }

  return (memory+address);