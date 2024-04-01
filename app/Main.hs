module Main where

-- import qualified GlossMain as GM
import qualified BrickMain as BM

import Chip8 (decode, dis, disPseudo, newChip8, newEmulator)

import Util (lpad, split, hexPad, hexInt, join, filterJust)

import qualified Data.ByteString.Lazy as BL
import Data.Binary.Get (Get, isEmpty, getWord16be, runGet)
import Data.Word (Word16)
import Data.Maybe (isJust)
import System.Random (randomRs, mkStdGen, randomIO)
import System.Environment (getArgs)
import System.Exit (exitSuccess)

-- Reads a 16-bit big endian word from the input.
getWord16beList :: Get [Word16]
getWord16beList =  (go [])
  where
    go acc = do
      empty <- isEmpty
      if empty
        then return acc
        else do
          word <- getWord16be
          rest <- getWord16beList
          go (word : rest)

main :: IO ()
main = getArgs >>= parse

parse ["-h"] = usage >> exit
parse ["dis", file] = cliDis dis file
parse ["pseudo", file] = cliDis disPseudo file
parse ["brick", file] = brick file []
parse ["brick", file, "-b", b] = brick file b
parse [] = brick "resources/octojam1title.ch8" []
parse _ = usage >> exit

usage = putStrLn "Usage: chip8 [brick <filename>]"
exit = exitSuccess

-- Disassemble with a given dis function
cliDis :: (Word16 -> String) -> String -> IO ()
cliDis f file = do
  rom  <- BL.readFile file
  let prog = runGet getWord16beList rom
  (putStrLn . join "\n") [
    hexPad 3 (0x200 + addr*2) ++ " " ++ ins
    | (ins, addr) <- zip (map f prog) [0..]
    ]

parseBreakpoints :: String -> [Int]
parseBreakpoints = filterJust . map hexInt . split ","

brick :: String -> String -> IO ()
brick file bs = do
  rom  <- BL.readFile file
  seed <- randomIO
  let rng = randomRs (0, 255) (mkStdGen seed)
  let vm  = newChip8 (BL.unpack rom) rng
  let emu = newEmulator vm (parseBreakpoints bs)
  
  BM.run emu