module Main where

-- import qualified GlossMain as GM
import qualified BrickMain as BM

import Chip8 (decode, dis, disPseudo, newChip8, newEmulator)

import Util (lpad, split, hexPad, hexInt, join, filterJust, splitN)

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

parseBreakpoints :: String -> [Int]
parseBreakpoints = filterJust . map hexInt . split ","

-- We can model arg parsing as a sequence of mutations on a model given
--  the remaining arguments and some communication of how many of those
--  arguments were consumed.
argparse :: (a -> [String] -> (a, [String])) -> a -> [String] -> a
argparse _ acc [] = acc
argparse eat acc args
  -- Sanity check, make sure we're actually consuming the args
  | length args > length args' = argparse eat acc' args'
  | otherwise = acc'
  where (acc', args') = eat acc args

data BrickArgs = BrickArgs {
  baBPs :: [Int],  -- List of breakpoints
  baRes :: String, -- Resolution type
  baFg  :: String, -- Foreground color
  baBg  :: String  -- Background color
}
  deriving (Show)

-- It gets us this nice syntax here
brick_ap :: [String] -> BrickArgs
brick_ap = argparse go (BrickArgs [] "" "" "")
  where
    go bap ("-b":b:args) = (bap { baBPs = parseBreakpoints b }, args)
    go bap ("-r":res:args) = (bap { baRes = res }, args)
    go bap ("-c":fg_bg:args) = (bap', args)
      where
        xs = splitN 1 "," fg_bg
        bap' = bap {
          baFg = if length xs > 0 then xs!!0 else "",
          baBg = if length xs > 1 then xs!!1 else ""
        }
    
    go bap [] = (bap, [])
    go bap (x:xs) = (bap, xs)

parse ["-h"] = usage >> exit
parse ["dis", file] = cliDis dis file
parse ["pseudo", file] = cliDis disPseudo file
parse ("brick":file:opts) = brick file (brick_ap opts)

parse [] = brick "resources/octojam1title.ch8" (brick_ap [])
parse _ = usage >> exit

usage = putStrLn "Usage: cabal run exes -- cmd ...\n\
  \  (nothing)                                  Run example in brick with no breakpoints.\n\
  \  dis    <file>                              Disassemble a file.\n\
  \  pseudo <file>                              Generate pseudocode disassembly.\n\
  \  brick  <file> [-b bs] [-r res] [-c fg,bg]  Run in brick.\n\
  \    bs    Comma-separated list of hex breakpoints.\n\
  \    res   Resolution/render type, one of: ascii, braille, 1x1, 1x2, 2x2, 2x3. Default 2x3\n\
  \    fg    Foreground color (red, blue, green, yellow, white, black, magenta, cyan, #xxx, or #xxxxxx)\n\
  \    bg    Background color"
exit = exitSuccess

-- Disassemble with a given dis function
cliDis :: (Word16 -> String) -> String -> IO ()
cliDis df file = do
  rom  <- BL.readFile file
  let prog = runGet getWord16beList rom
  (putStrLn . join "\n") [
    hexPad 3 (0x200 + addr*2) ++ " " ++ df ins
    | (addr, ins) <- zip [0..] prog
    ]

brick :: String -> BrickArgs -> IO ()
brick file args@(BrickArgs bs rt fg bg) = do
  rom  <- BL.readFile file
  seed <- randomIO
  putStrLn (show args)
  let
    rng = randomRs (0, 255) (mkStdGen seed)
    vm  = newChip8 (BL.unpack rom) rng
    emu = newEmulator vm bs
  BM.run rt fg bg emu