{-# LANGUAGE BlockArguments #-}

{-
CHIP-8 is an interpreted virtual machine designed for portable video games.
 It has a much simpler design than most actual hardware and a number of games
 and opcode extensions created for it. The base design has 35 opcodes with
 fixed 16-bit big-endian codes.

References
 - https://en.wikipedia.org/wiki/CHIP-8
 - https://chip-8.github.io/database/
 - https://github.com/gcsmith/gchip/
 -}

module Chip8 where

import Control.Monad.State (State, get, gets, modify, when, execState)
import Data.Vector.Unboxed (Vector, (//), (!), accum, generate, slice)
import qualified Data.Vector.Unboxed as V (length)
import Data.Bits ((.|.), (.&.), xor, shiftL, shiftR, testBit, setBit, clearBit)
import Data.Word (Word8, Word16)
import Data.Maybe (isJust, isNothing)
import Data.Char (toLower)

import Util (hexPad, vReg, m1)

type Vec = Vector

-- Low font, for hex displays
lfont :: [Word8]
lfont = [
  0xF0, 0x90, 0x90, 0x90, 0xF0, 0x20, 0x60, 0x20, 0x20, 0x70, -- 0, 1
  0xF0, 0x10, 0xF0, 0x80, 0xF0, 0xF0, 0x10, 0xF0, 0x10, 0xF0, -- 2, 3
  0x90, 0x90, 0xF0, 0x10, 0x10, 0xF0, 0x80, 0xF0, 0x10, 0xF0, -- 4, 5
  0xF0, 0x80, 0xF0, 0x90, 0xF0, 0xF0, 0x10, 0x20, 0x40, 0x40, -- 6, 7
  0xF0, 0x90, 0xF0, 0x90, 0xF0, 0xF0, 0x90, 0xF0, 0x10, 0xF0, -- 8, 9
  0xF0, 0x90, 0xF0, 0x90, 0x90, 0xE0, 0x90, 0xE0, 0x90, 0xE0, -- A, B
  0xF0, 0x80, 0x80, 0x80, 0xF0, 0xE0, 0x90, 0x90, 0x90, 0xE0, -- C, D
  0xF0, 0x80, 0xF0, 0x80, 0xF0, 0xF0, 0x80, 0xF0, 0x80, 0x80] -- E, F

-- Height of each character
lfont_height :: Int
lfont_height = 5

-- State of the whole VM
data Chip8 = Chip8 {
  screen ::  Vec Bool,    -- Screen map, 64x32 monochrome
  memory ::  Vec Word8,   -- Memory
  rV     ::  Vec Word8,   -- 16 registers
  rI     ::  Word16,      -- 12-bit address register
  rPC    ::  Word16,      -- Program Counter
  stack  :: [Word16],     -- Stack (opaque to program)
  keys   ::  Word16,      -- 16 keys
  waitR  ::  Maybe Word8, -- Set to VX when awaiting a key press
  delayT ::  Word8,       -- Delay timer, counts down at 60 Hz
  soundT ::  Word8,       -- Sound timer
  rng    :: [Int],        -- Infinite list of random numbers
  width  ::  Int,         -- Width of the screen
  height ::  Int,         -- Height of the screen
  cycles ::  Int          -- Total cycles of the VM
}

-- Construct a new VM
newChip8 :: [Word8] -> [Int] -> Chip8
newChip8 rom rand = Chip8 {
  rV     = generate 16 (const 0),
  rI     = 0x000,
  rPC    = 0x200, -- low 512 bytes reserved
  stack  = [],
  memory = generate 0x1000 (const 0) // (
    zip [0    ..] lfont ++
    zip [0x200..] rom
  ),
  keys   = 0,
  waitR  = Nothing,
  delayT = 0,
  soundT = 0,
  rng    = rand,
  screen = generate (w*h) (const False),
  width  = w,
  height = h,
  cycles = 0
}
  where
    w = 64
    h = 32

-- Get the given V register
getV :: Word8 -> State Chip8 Word8
getV x = do
  v <- gets rV
  return (v!fromIntegral x)

-- Set the given V register
setV :: Word8 -> Word8 -> State Chip8 ()
setV x value = do
  v <- gets rV
  modify $ \s -> s { rV = v//[(fromIntegral x, value)] }

-- Apply a modifying function to the given V register
modifyV :: Word8 -> (Word8 -> Word8) -> State Chip8 ()
modifyV x f = do
  old <- getV x
  setV x (f old)

-- Apply a modifying function to the PC
modifyPC :: (Word16 -> Word16) -> State Chip8 ()
modifyPC f = do
  old <- gets rPC
  modify $ \s -> s { rPC = f old }

-- Read a word from memory
readWord :: Integral a => a -> Chip8 -> Word16
readWord addr vm = fromIntegral word
  where
    ia   = fromIntegral addr
    mem  = memory vm
    hi   = fromIntegral $ mem!(ia  ) :: Int
    lo   = fromIntegral $ mem!(ia+1) :: Int
    word = (hi `shiftL` 8) .|. lo

-- Embed a position into an offset into the 1D screen vector
screenIndex :: Int -> Int -> Chip8 -> Int
screenIndex x y vm = yy*(width vm) + xx
  where
    xx = x `mod` width  vm
    yy = y `mod` height vm

-- Get the value of a pixel at a given position
pixel :: Chip8 -> Int -> Int -> Bool
pixel vm x y = screen vm ! screenIndex x y vm

-- Blit a sprite to the screen
bitBlit :: Integral a => a -> a -> Vec Word8 -> State Chip8 Bool
bitBlit x y spr = do
  vm  <- get
  let
    ix  = fromIntegral x `mod` width  vm :: Int
    iy  = fromIntegral y `mod` height vm :: Int
    scr = screen vm
    sw  = 8
    sh  = V.length spr    
    updates = [ -- Expand into (index, bit) pairs
      (screenIndex (ix + i) (iy + j) vm, (spr!j) `testBit` fromIntegral (7 - i))
      | i <- m1 [1..sw],
        j <- m1 [1..sh]
      ]
  modify $ \s -> s { screen = accum xor scr updates }
  -- Since we have the updates list, we can ask if a collision occurred
  return $ any (\(i, u) -> (scr!i) && u) updates

{- Operations -}

data Op =
    BAD -- Invalid instruction
  | CLS -- CLear Screen
  | RET -- RETurn
  | JMP -- JuMP to address
  | JSR -- Jump to SubRoutine
  | SEQ -- Skip if Equal
  | SNE -- Skip if Not Equal
  | MOV -- MOVe
  | ADD -- ADD without carry
  | IOR -- bitwise Inclusive OR
  | AND -- bitwise AND
  | XOR -- bitwise eXclusive OR
  | ADC -- AdD register with Carry
  | SXY -- SuBtract VX and VY with carry
  | LSR -- Logical Shift Right with carry
  | SYX -- Subtract VX and VY (VX = VY - VX)
  | ASL -- Arithmetic Shift Left with carry
  | LDI -- LoaD address to I register
  | VJP -- Virtual JumP (PC = V0 + NNN)
  | RND -- RaNDom with mask
  | DRW -- Draw an 8xN sprite at (VX, VY)
  | SKE -- Skip if Key Equals register
  | SKN -- Skip if Key Not equals register
  | RDD -- ReaD Delay
  | RDK -- await and ReaD a Key press
  | WRD -- WRite Delay
  | WRS -- WRite Sound
  | ADI -- AdD VX to I register
  | SPR -- load SPRite into I register
  | BCD -- store the BCD in I[:2]
  | STR -- STore Registers V0 to VX into I
  | LDR -- LoaD Registers V0 to VX into I
  deriving (Show, Eq)

-- Operands to be paired with an operation
data Operands =
    OpNone
  | OpX    Word8
  | OpXY   Word8 Word8
  | OpXYN  Word8 Word8 Word8
  | OpXNN  Word8 Word8
  | OpNNN  Word16
  | OpNNNN Word16 -- Used for "bad" instructions
  deriving (Show, Eq)

-- Instruction, an operation and its operands
data Ins = Ins Op Operands
  deriving (Show, Eq)

{- High level applications -}

-- Binary operations
applyBinary :: (Int -> Int -> Int) -> Operands -> State Chip8 ()
applyBinary f (OpXY x y) = do
  vx <- getV x
  vy <- getV y
  setV x (fromIntegral $ f (fromIntegral vx) (fromIntegral vy))

applyBinary f (OpXNN x nn) = do
  vx <- getV x
  setV x (fromIntegral $ f (fromIntegral vx) (fromIntegral nn))

applyBinary _ _ = undefined

-- Operations which set VF to a carry
applyUnaryCarry :: (Int -> (Int, Bool)) -> Operands -> State Chip8 ()
applyUnaryCarry f (OpX x) = do
  vx <- getV x
  let (result, carry) = f (fromIntegral vx)
  setV   x (fromIntegral result)
  setV 0xF ((fromIntegral . fromEnum) carry)

applyUnaryCarry _ _ = undefined

applyBinaryCarry :: (Int -> Int -> (Int, Bool)) -> Operands -> State Chip8 ()
applyBinaryCarry f (OpXY x y) = do
  vx <- getV x
  vy <- getV y
  let (result, carry) = f (fromIntegral vx) (fromIntegral vy)
  setV   x (fromIntegral result)
  setV 0xF ((fromIntegral . fromEnum) carry)

applyBinaryCarry f (OpXNN x nn) = do
  vx <- getV x
  let (result, carry) = f (fromIntegral vx) (fromIntegral nn)
  setV   x (fromIntegral result)
  setV 0xF ((fromIntegral . fromEnum) carry)

applyBinaryCarry _ _ = undefined

-- Boolean operations which skip the next instruction
applySkip :: (Int -> Int -> Bool) -> Operands -> State Chip8 ()
applySkip p (OpXY x y) = do
  vx <- getV x
  vy <- getV y
  let skip = p (fromIntegral vx) (fromIntegral vy)
  when skip $ modifyPC (+2)

applySkip p (OpXNN x nn) = do
  vx <- getV x
  let skip = p (fromIntegral vx) (fromIntegral nn)
  when skip $ modifyPC (+2)

applySkip _ _ = undefined

{- Applications -}

-- These are curried instead of matched with Ins for brevity and because
--  apply is meant to be internal anyway.

-- Apply the given operation and operands to the VM
apply :: Op -> Operands -> State Chip8 ()

apply CLS (OpNone) = do
  w <- gets width
  h <- gets height
  modify $ \s -> s { screen = generate (w*h) (const False) }

apply RET (OpNone) = do
  hss <- gets stack
  case hss of
    (h:ss) -> modify $ \s -> s { rPC = h, stack = ss }
    []     -> error "Stack is empty"

-- Minus 2 so the implicit PC advance gets to the right place
apply JMP (OpNNN nnn) = modify $ \s -> s { rPC = nnn - 2 }

apply JSR (OpNNN nnn) = do
  old <- gets rPC
  ss  <- gets stack
  -- -2 so the implicit +2 gets to the right instruction
  modify $ \s -> s { rPC = nnn - 2, stack = old:ss }

-- Eta reduction doesn't work with mixed function-level pattern matching
apply SEQ z = applySkip (==) z
apply SNE z = applySkip (/=) z
apply MOV z = applyBinary (\_ y -> y) z
apply ADD (OpXNN x nn) = modifyV x (+nn)

apply IOR z = applyBinary (.|.) z
apply AND z = applyBinary (.&.) z
apply XOR z = applyBinary (xor) z

apply SXY z = applyBinaryCarry (\x y -> (x - y, x >= y)) z
apply SYX z = applyBinaryCarry (\x y -> (y - x, y >= x)) z
apply ADC z = applyBinaryCarry (\x y -> let r = x + y in (r, r > 0xFF)) z
apply LSR z = applyUnaryCarry  (\x   -> (x `shiftR` 1, x `testBit` 0))  z
apply ASL z = applyUnaryCarry  (\x   -> (x `shiftL` 1, x `testBit` 7))  z

apply LDI (OpNNN nnn) = modify $ \s -> s { rI = nnn }

apply VJP (OpNNN nnn) = do
  v0 <- getV 0
  modify $ \s -> s { rPC = fromIntegral v0 + nnn }

apply RND (OpXNN x nn) = do
  -- rng is an infinite list, but Haskell doesn't know that
  rng' <- gets rng
  case rng' of
    [] -> undefined
    (rn:rng'') -> do
      setV x (fromIntegral rn .&. nn)
      modify $ \s -> s { rng = rng'' }

apply DRW (OpXYN x y n) = do
  vx  <- getV x
  vy  <- getV y
  spr <- gets rI
  mem <- gets memory
  let
    ss = fromIntegral spr
    ni = fromIntegral n
    nn = if ni == 0 then 16 else ni
  collision <- bitBlit vx vy (slice ss nn mem)
  setV 0xF ((fromIntegral . fromEnum) collision)

apply SKE (OpX x) = do
  vx <- getV x
  k  <- gets keys
  let key = fromIntegral (vx .&. 0xF)
  when (k `testBit` key) $ modifyPC (+2)

apply SKN (OpX x) = do
  vx <- getV x
  k  <- gets keys
  let key = fromIntegral (vx .&. 0xF)
  when (not $ k `testBit` key) $ modifyPC (+2)

apply RDD (OpX x) = do
  delay <- gets delayT
  setV x delay

apply RDK (OpX x) = do
  modify $ \s -> s { waitR = Just x }

apply WRD (OpX x) = do
  vx <- getV x
  modify $ \s -> s { delayT = vx }

apply WRS (OpX x) = do
  vx <- getV x
  modify $ \s -> s { soundT = vx }

apply ADI (OpX x) = do
  i  <- gets rI
  vx <- getV x
  modify $ \s -> s { rI = i + fromIntegral vx }

apply SPR (OpX x) = do
  vx <- getV x
  let addr = fromIntegral ((vx .&. 0xF)*fromIntegral lfont_height)
  modify $ \s -> s { rI = addr }

apply BCD (OpX x) = do
  i   <- gets rI
  vx  <- getV x
  mem <- gets memory
  let (d', d0) = vx `divMod` 10
  let (d2, d1) = d' `divMod` 10
  let ii = fromIntegral i
  modify $ \s -> s { memory = mem//zip [ii..] [d2, d1, d0] }

apply STR (OpX x) = do
  i   <- gets rI
  v   <- gets rV
  mem <- gets memory
  let
    ii = fromIntegral i
    ix = fromIntegral x
  modify $ \s -> s { memory = mem//zip [ii..] (map (v!) [0..ix]) }

apply LDR (OpX x) = do
  i   <- gets rI
  v   <- gets rV
  mem <- gets memory
  let
    ii = fromIntegral i
    ix = fromIntegral x
  modify $ \s -> s { rV = v//zip [0..ix] (map (mem!) [ii..]) }

apply op erands = do
  pc <- gets rPC
  error ("apply " ++ show op ++ " " ++ show erands ++ " @" ++ hexPad 3 pc)

-- Encode just the operation using operands to disambiguate
encOp :: Op -> Operands -> Word16

-- This looks pretty with the fields named, but the compiler complains about
--  unused matches :(
encOp CLS (OpNone     ) = 0x00E0
encOp RET (OpNone     ) = 0x00EE

encOp JMP (OpNNN     _) = 0x1000
encOp JSR (OpNNN     _) = 0x2000
encOp SEQ (OpXNN _   _) = 0x3000
encOp SNE (OpXNN _   _) = 0x4000
encOp SEQ (OpXY  _ _  ) = 0x5000
encOp MOV (OpXNN _   _) = 0x6000
encOp ADD (OpXNN _   _) = 0x7000
encOp MOV (OpXY  _ _  ) = 0x8000
encOp SNE (OpXY  _ _  ) = 0x9000

encOp IOR (OpXY  _ _  ) = 0x8001
encOp AND (OpXY  _ _  ) = 0x8002
encOp XOR (OpXY  _ _  ) = 0x8003
encOp ADC (OpXY  _ _  ) = 0x8004
encOp SXY (OpXY  _ _  ) = 0x8005
encOp LSR (OpX   _    ) = 0x8006
encOp SYX (OpXY  _ _  ) = 0x8007
encOp ASL (OpX   _    ) = 0x800E

encOp LDI (OpNNN     _) = 0xA000
encOp VJP (OpNNN     _) = 0xB000
encOp RND (OpXNN _   _) = 0xC000
encOp DRW (OpXYN _ _ _) = 0xD000

encOp SKE (OpX   _    ) = 0xE09E
encOp SKN (OpX   _    ) = 0xE0A1

encOp RDD (OpX   _    ) = 0xF007
encOp RDK (OpX   _    ) = 0xF00A
encOp WRD (OpX   _    ) = 0xF015
encOp WRS (OpX   _    ) = 0xF018
encOp ADI (OpX   _    ) = 0xF01E
encOp SPR (OpX   _    ) = 0xF029
encOp BCD (OpX   _    ) = 0xF033
encOp STR (OpX   _    ) = 0xF055
encOp LDR (OpX   _    ) = 0xF065

encOp _ _ = undefined

-- Encode operands into a word
encOperands :: Operands -> Word16
encOperands operands = case operands of
  (OpNone     ) -> 0x0000
  (OpX   x    ) -> fromIntegral (vx x)
  (OpXY  x y  ) -> fromIntegral (vx x .|. vy y)
  (OpXYN x y n) -> fromIntegral (vx x .|. vy y .|. nibble n)
  (OpXNN x  nn) -> fromIntegral (vx x .|. fromIntegral nn)
  (OpNNN   nnn) -> nnn .&. 0xFFF
  (OpNNNN nnnn) -> nnnn
  where
    nibble n = (fromIntegral n .&. 0xF) :: Int
    vx x = fromIntegral $ (nibble x) `shiftL` 8 :: Int
    vy y = fromIntegral $ (nibble y) `shiftL` 4 :: Int

-- Encode an instruction into a word
encode :: Ins -> Word16
encode (Ins op erands) = encOp op erands .|. encOperands erands

{- Operand decoding -}

type DecodeOperands = Word16 -> Operands
decodeNone, decodeX, decodeXY, decodeXYN, decodeXNN, decodeNNN, decodeNNNN :: DecodeOperands

decodeNone _    = OpNone
decodeX    code = OpX       (fromIntegral ((code .&. 0x0F00) `shiftR` 8))
decodeXY   code = OpXY  x   (fromIntegral ((code .&. 0x00F0) `shiftR` 4))
  where (OpX  x  ) = decodeX  code

decodeXYN  code = OpXYN x y (fromIntegral  (code .&. 0x000F))
  where (OpXY x y) = decodeXY code

decodeXNN  code = OpXNN x   (fromIntegral  (code .&. 0x00FF))
  where (OpX  x  ) = decodeX  code

decodeNNN  code = OpNNN     (fromIntegral  (code .&. 0x0FFF))

decodeNNNN code = OpNNNN                    code

-- Decode a word into an instruction
decode :: Word16 -> Ins
decode 0x00E0 = Ins CLS OpNone
decode 0x00EE = Ins RET OpNone
decode code   = Ins op (de code)
  where
    -- Split word into 0xabcd
    a  = code `shiftR` 12
    d  = code .&. 0xF
    cd = code .&. 0xFF
    (op, de) = case a of
      0x0 -> (BAD, decodeNNNN)
      -- Other 0 prefix codes unused
      0x1 -> (JMP, decodeNNN)
      0x2 -> (JSR, decodeNNN)
      0x3 -> (SEQ, decodeXNN)
      0x4 -> (SNE, decodeXNN)
      0x5 -> case d of
        0x0 -> (SEQ, decodeXY)
        -- 5001:500F unused
        _   -> (BAD, decodeNNNN)
      0x6 -> (MOV, decodeXNN)
      0x7 -> (ADD, decodeXNN)
      0x8 -> case d of
        0x0 -> (MOV, decodeXY)
        0x1 -> (IOR, decodeXY)
        0x2 -> (AND, decodeXY)
        0x3 -> (XOR, decodeXY)
        0x4 -> (ADC, decodeXY)
        0x5 -> (SXY, decodeXY)
        0x6 -> (LSR, decodeX )
        0x7 -> (SYX, decodeXY)
        -- 8008:800D unused
        0xE -> (ASL, decodeX )
        -- 800F unused
        _   -> (BAD, decodeNNNN)
      0x9 -> (SNE, decodeXY )
      0xA -> (LDI, decodeNNN)
      0xB -> (VJP, decodeNNN)
      0xC -> (RND, decodeXNN)
      0xD -> (DRW, decodeXYN)
      0xE -> case cd of
        0x9E -> (SKE, decodeX)
        0xA1 -> (SKN, decodeX)
        -- Other E prefix codes unused
        _    -> (BAD, decodeNNNN)
      0xF -> case cd of
        0x07 -> (RDD, decodeX)
        0x0A -> (RDK, decodeX)
        0x15 -> (WRD, decodeX)
        0x1E -> (ADI, decodeX)
        0x29 -> (SPR, decodeX)
        0x33 -> (BCD, decodeX)
        0x55 -> (STR, decodeX)
        0x65 -> (LDR, decodeX)
        -- Other F prefix codes unused
        _    -> (BAD, decodeNNNN)
      -- Should never happen
      _ -> (BAD, decodeNNNN)

-- Disassemble an operation
disOp :: Op -> String
disOp op = map toLower (show op)

-- Utility for disassembling with hex
disHex :: Integral a => a -> String
disHex n = " 0x" ++ hexPad 3 n

-- Disassemble an operands
disOperands :: Op -> Operands -> String

-- Disassemble with hex for some opcodes
disOperands BAD (OpNNNN nnnn) = "0x" ++ hexPad 4 nnnn
disOperands JMP (OpNNN   nnn) = disHex nnn
disOperands JSR (OpNNN   nnn) = disHex nnn
disOperands LDI (OpNNN   nnn) = disHex nnn
disOperands VJP (OpNNN   nnn) = disHex nnn
disOperands RND (OpXNN _  nn) = disHex nn

disOperands _ erands = case erands of
  (OpNone     ) -> ""
  (OpX   x    ) -> " " ++ vReg x
  (OpXY  x y  ) -> " " ++ vReg x ++ " " ++ vReg y
  (OpXYN x y n) -> " " ++ vReg x ++ " " ++ vReg y ++ " " ++ show n
  (OpXNN x  nn) -> " " ++ vReg x ++ " " ++ show nn
  (OpNNN   nnn) -> " " ++ show nnn
  (OpNNNN nnnn) -> "0x" ++ hexPad 2 nnnn

-- Disassemble a single opcode
dis :: Word16 -> String
dis code
  | op == BAD = "0x" ++ hexPad 4 code
  | otherwise = disOp op ++ disOperands op erands
  where (Ins op erands) = decode code

-- Format `vx op= ...`
eqop :: String -> Operands -> String
eqop op (OpXNN x nn) = vReg x ++ " " ++ op ++ "= " ++ show nn
eqop op (OpXY  x  y) = vReg x ++ " " ++ op ++ "= " ++ vReg y
eqop _ _ = undefined

-- Render it in pseudocode (I'm not great at assembly)
pseudo :: Op -> Operands -> String
pseudo BAD (OpNNNN nnnn) = "0x" ++ hexPad 4 nnnn
pseudo CLS (OpNone     ) = "clear()"
pseudo RET (OpNone     ) = "return"
pseudo JMP (OpNNN   nnn) = "goto *0x" ++ hexPad 3 nnn
pseudo JSR (OpNNN   nnn) = "call *0x" ++ hexPad 3 nnn
pseudo SEQ  op           = "if(" ++ eqop "!" op ++ ")"
pseudo SNE  op           = "if(" ++ eqop "=" op ++ ")"
pseudo MOV  op           = eqop ":" op
pseudo ADD  op           = eqop "+" op
pseudo IOR  op           = eqop "|" op
pseudo AND  op           = eqop "&" op
pseudo XOR  op           = eqop "^" op
pseudo ADC  op           = eqop "+" op
pseudo SXY  op           = eqop "-" op
pseudo LSR (OpX   x    ) = vReg x ++ " <<= 1"
pseudo SYX (OpXY  x y  ) = vReg x ++ " = " ++ vReg y ++ " - " ++ vReg x
pseudo ASL (OpX   x    ) = vReg x ++ " >>= 1"
pseudo LDI (OpNNN   nnn) = "I = 0x" ++ hexPad 3 nnn
pseudo VJP (OpNNN   nnn) = "goto *(V0 + " ++ show nnn ++ ")"
pseudo RND (OpXNN x  nn) = vReg x ++ " = rand() & 0x" ++ hexPad 2 nn
pseudo DRW (OpXYN x y n) = "draw(x=" ++ vReg x ++ ", y=" ++ vReg y ++ ", h=" ++ show n ++ ")"
pseudo SKE (OpX   x    ) = "if(!key[" ++ vReg x ++ "])"
pseudo SKN (OpX   x    ) = "if(key[" ++ vReg x ++ "])"
pseudo RDD (OpX   x    ) = vReg x ++ " = delay"
pseudo RDK (OpX   x    ) = vReg x ++ " = key()"
pseudo WRD (OpX   x    ) = "delay = " ++ vReg x
pseudo WRS (OpX   x    ) = "sound = " ++ vReg x
pseudo ADI (OpX   x    ) = "I += " ++ vReg x
pseudo SPR (OpX   x    ) = "I = sprite[" ++ vReg x ++ "]"
pseudo BCD (OpX   x    ) = "I[:2] = BCD(" ++ vReg x ++ ")"
pseudo STR (OpX   x    ) = "I[:" ++ show x ++ "] = V[:" ++ show x ++ "]"
pseudo LDR (OpX   x    ) = "V[:" ++ show x ++ "] = I[:" ++ show x ++ "]"

pseudo _ _ = "???"

-- Disassemble into pseudocode
disPseudo :: Word16 -> String
disPseudo code = pseudo op erands
  where (Ins op erands) = decode code

-- Clear all keys
clearKeys :: Chip8 -> Chip8
clearKeys vm = vm { keys = 0 }

-- Receive a key event
keyEvent :: Int -> Bool -> Chip8 -> Chip8
keyEvent kev press = execState do
  waitr <- gets waitR
  case waitr of
    Nothing -> pure () -- Not waiting
    Just kr -> setV kr (fromIntegral kev)
  
  k <- gets keys
  modify $ \s -> s {
    keys  = (if press then setBit else clearBit) k kev,
    waitR = Nothing
  }

-- Execute the VM for one cycle
execute :: Chip8 -> Chip8
execute = execState do
  waitr <- gets waitR
  -- If we're not waiting, run the VM
  when (isNothing waitr) do
    pc   <- gets rPC
    code <- gets (readWord pc)
    let (Ins op erands) = decode code
    apply op erands
    modifyPC (+2) -- Don't reuse pc, it may have changed
    cyc  <- gets cycles
    modify $ \s -> s { cycles = cyc + 1 }

-- Call at a rate of 60 Hz
countdown :: Chip8 -> Chip8
countdown = execState do
  dt <- gets delayT
  st <- gets soundT
  modify $ \s -> s {
    delayT = if dt == 0 then 0 else dt - 1,
    soundT = if st == 0 then 0 else st - 1
  }

-- Emulator status
data Status =
    Running -- Normal running state
  | Paused  -- Emulator is paused
  | Resumed -- Emulator is resumed (ignore breakpoints for 1 cycle)
  deriving (Show, Eq)

-- Separate the VM from the emulator, which keeps track of a number of
--  states to allow rewinding and forwarding.
data Emulator = Emulator {
  states :: [Chip8], -- List of recorded states
  breaks :: [Int],   -- List of breakpoints
  stPos  ::  Int,    -- Which state is current
  maxPos ::  Int,    -- Maximum saved states
  status ::  Status  -- Emulator status
}

-- Get the latest state
latest :: Emulator -> Chip8
latest = head . states

-- Get the current state (respecting stPos)
current :: Emulator -> Chip8
current emu = states emu !! stPos emu

-- Construct a new emulator
newEmulator :: Chip8 -> [Int] -> Emulator
newEmulator state bs = Emulator {
  states = [state],
  breaks = bs,
  stPos  = 0,
  maxPos = 1024,
  status = Running
}

-- Pause the emulator
pause :: Emulator -> Emulator
pause emu = emu { status = Paused }

-- Toggle whether the emulator is paused
togglePause :: Emulator -> Emulator
togglePause emu = (if status emu == Paused then resume else pause) emu

-- Try to rewind the emulator to the next oldest state
rewind :: Emulator -> Emulator
rewind emu = pause emu {
  stPos = min (stPos emu + 1) (length (states emu) - 1)
}

-- Try to move to the next state
forward :: Emulator -> Emulator
forward emu
  -- If we're at the forefront, continue executing
  | stPos emu == 0 = forceUpdate execute pemu
  | otherwise = pemu {
    stPos = max (stPos emu - 1) 0
  }
  where pemu = pause emu

-- Resume the emulator after it was paused
resume :: Emulator -> Emulator
resume emu = emu {
  stPos  = 0, -- overwrite any future states
  states = drop (stPos emu) (states emu),
  status = Resumed
}

-- Update even if paused
forceUpdate :: (Chip8 -> Chip8) -> Emulator -> Emulator
forceUpdate todo emu = emu {
  states = take (maxPos emu) $ todo (latest emu):states emu
}

-- Append a new updated state
update :: (Chip8 -> Chip8) -> Emulator -> Emulator
update todo emu = case status emu of
  Paused -> emu -- Do nothing when paused
  _      -> forceUpdate todo emu {
    status = Running -- Successful update is always running
  }

-- Run the emulator for one cycle
emulate :: Emulator -> Emulator
emulate = execState do
  st <- gets status
  when (st /= Paused) do
    bs <- gets breaks
    pc <- gets (fromIntegral . rPC . latest)
    
    if st /= Resumed && pc `elem` bs then
      modify pause -- Reached a breakpoint
    else
      modify (update execute)