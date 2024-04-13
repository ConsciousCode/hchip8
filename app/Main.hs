module Main where

-- import qualified GlossMain as GM
import qualified BrickMain as BM

import Chip8 (dis, disPseudo, newChip8, newEmulator)

import Util (split, hexPad, hexInt, join, filterJust, splitN)

import qualified Data.ByteString.Lazy as BL
import Data.Binary.Get (Get, isEmpty, getWord16be, runGet)
import Data.Word (Word16)
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

-- It gets us this nice syntax here
brick_ap :: [String] -> BM.Args
brick_ap = argparse go (BM.Args [] "" "" "")
  where
    go bap ("-b":b:args) = (bap { BM.baBPs = parseBreakpoints b }, args)
    go bap ("-r":res:args) = (bap { BM.baRes = res }, args)
    go bap ("-c":fg_bg:args) = (bap', args)
      where
        xs = splitN 1 "," fg_bg
        bap' = bap {
          BM.baFg = if length xs > 0 then xs!!0 else "",
          BM.baBg = if length xs > 1 then xs!!1 else ""
        }
    
    go bap [] = (bap, [])
    go bap (_:xs) = (bap, xs)

parse :: [String] -> IO ()
parse ["-h"] = usage >> exit
parse ["dis", file] = cliDis dis file
parse ["pseudo", file] = cliDis disPseudo file
parse ("brick":file:opts) = brick file (brick_ap opts)

parse [] = brick "resources/octojam1title.ch8" (brick_ap [])
parse _ = usage >> exit

usage :: IO ()
usage = putStrLn "Usage: cabal run exes -- cmd ...\n\
  \  (nothing)                                  Run example in brick with no breakpoints.\n\
  \  dis    <file>                              Disassemble a file.\n\
  \  pseudo <file>                              Generate pseudocode disassembly.\n\
  \  brick  <file> [-b bs] [-r res] [-c fg,bg]  Run in brick.\n\
  \    bs    Comma-separated list of hex breakpoints.\n\
  \    res   Resolution/render type, one of:\n\
  \      ascii1x1, ascii1x2, ascii2x2 (not recommended)\n\
  \      braille, 1x1, 1x2, 2x2, 2x3. Default 2x3.\n\
  \    fg    Foreground color, one of:\n\
  \      red, blue, green, yellow,\n\
  \      white, black, magenta, cyan,\n\
  \      #xxx, #xxxxxx. Default white.\n\
  \    bg    Background color. Default black."

exit :: IO ()
exit = exitSuccess

-- Disassemble with a given dis function
cliDis :: (Word16 -> String) -> String -> IO ()
cliDis df file = do
  rom  <- BL.readFile file
  let prog = runGet getWord16beList rom
  (putStrLn . join "\n") [
    hexPad 3 (0x200 + addr*2 :: Int) ++ " " ++ df ins
    | (addr, ins) <- zip [0..] prog
    ]

brick :: String -> BM.Args -> IO ()
brick file args = do
  rom  <- BL.readFile file
  seed <- randomIO
  let
    rng = randomRs (0, 255) (mkStdGen seed)
    vm  = newChip8 (BL.unpack rom) rng
    emu = newEmulator vm (BM.baBPs args)
  BM.run args emu