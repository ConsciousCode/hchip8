module Util where

import Data.Char (ord, isDigit, isHexDigit, digitToInt, toLower)
import Data.Maybe (Maybe(Nothing, Just), isJust)

{-
General utilities - can you tell I like Python?
 -}

-- Handy function from HW3 with a couple changes:
--  - predicate continues instead of halts
--  - tail recursive
unfold :: (b -> Bool) -> (b -> a) -> (b -> b) -> b -> [a]
unfold p h t x = go x []
  where
    go y acc
      | p y       = go (t y) (h y : acc)
      | otherwise = acc

-- Generic base conversion
toBase :: Integral a => Int -> String -> a -> String
toBase _ ds 0 = [head ds]
toBase base ds x = unfold (>0) digit (`div` base) (fromIntegral x)
  where digit d = ds!!(fromIntegral d `mod` base)

-- Convert int to binary string
bin :: Integral a => a -> String
bin = toBase 2 "01"

-- Convert int to hex string
hex :: Integral a => a -> String
hex = toBase 16 (['0'..'9'] ++ ['a'..'f'])

hexDigit :: Char -> Maybe Int
hexDigit c
  | isDigit c    = Just $ digitToInt c
  | isHexDigit c = Just $ ord (toLower c) - ord 'a' + 10
  | otherwise    = Nothing

-- Customizable padding
-- f: space -> to wrap -> wrapped
-- fill: list to fill with
-- n: minimum size
-- s: list to pad
pad :: ([a] -> [a] -> [a]) -> [a] -> Int -> [a] -> [a]
pad f fill n s = f (concat $ replicate count fill) s
  where count = (n - length s) `div` length fill

-- Left pad a string
lpad :: [a] -> Int -> [a] -> [a]
lpad = pad (++)

-- Right pad a string
rpad :: [a] -> Int -> [a] -> [a]
rpad = pad (flip (++))

-- I did this so often I needed a util
hexPad :: Integral a => Int -> a -> String
hexPad n s = lpad "0" n $ hex s

-- Name a given V register
vReg :: Integral a => a -> String
vReg x = "v" ++ hex x

-- Intersperse a list with an element between each one
intersperse :: a -> [a] -> [a]
intersperse sep xs = (tail . concat) [[sep, x] | x <- xs]

-- Join string with a separator (generic but probably doesn't matter)
join :: [a] -> [[a]] -> [a]
join sep = concat . intersperse sep

-- Minus 1 (useful for when we want [0..(x-1)] ~ m1 [1..x])
m1 :: [Int] -> [Int]
m1 = map (subtract 1)

-- Plus 1
p1 :: [Int] -> [Int]
p1 = map (+1)

startsWith :: Eq a => [a] -> [a] -> Bool
startsWith [] _ = True
startsWith _ [] = False
startsWith (x:xs) (c:sub)
  | x == c    = startsWith xs sub
  | otherwise = False

endsWith :: Eq a => [a] -> [a] -> Bool
endsWith sub = startsWith (reverse sub) . reverse

-- Produce a list of every cons of a list
unconsAll :: [a] -> [[a]]
unconsAll = reverse . unfold ((/= 0) . length) id tail

-- Find the index of a sublist, else Nothing
find :: Eq a => [a] -> [a] -> Maybe Int
find needle haystack = foldl go Nothing $ zip [0..] (unconsAll haystack)
  where
    go Nothing (i, hs)
      | startsWith needle hs = Just i
      | otherwise            = Nothing
    go result _ = result

-- List all non-overlapping matches
findAll :: Eq a => [a] -> [a] -> [Int]
findAll needle haystack = reverse . snd $ findAll' haystack (0, [])
  where
    nl = length needle
    findAll' hs (x, acc) = case find needle hs of
      Nothing -> (x, acc) -- Nothing left to find, we're done
      Just i  -> findAll' (drop (i + nl) hs) (x + i + nl, (x + i):acc)

-- Slice a list [start, end)
slice :: Eq a => (Int, Int) -> [a] -> [a]
slice (start, end) = take (end - start) . drop start

-- A take function which has the option to just return the whole list (as if n=inf)
takeMaybe :: Maybe Int -> [a] -> [a]
takeMaybe Nothing  = id
takeMaybe (Just n) = take n

-- Core algorithm giving us the option to split N times
splitMaybe :: Eq a => Maybe Int -> [a] -> [a] -> [[a]]
splitMaybe mn sub ls = map (flip slice ls) $ takeMaybe mn $ zip (0:p1 xs) (xs ++ [length ls])
  where xs = findAll sub ls

-- Split a list by a given sublist, no limit
split :: Eq a => [a] -> [a] -> [[a]]
split = splitMaybe Nothing

-- Split a list by sublist up to N times 
splitN :: Eq a => Int -> [a] -> [a] -> [[a]]
splitN n = splitMaybe (Just (n + 1))

-- Parse a hex string into an int
hexInt :: String -> Maybe Int
hexInt = foldl go (Just 0)
  where
    go Nothing _ = Nothing
    go (Just acc) h = case hexDigit h of
      Nothing -> Nothing
      Just d  -> Just (acc*16 + d)

-- Filter a list of all Nothing and return Just unpacked
filterJust :: [Maybe a] -> [a]
filterJust ms = [maybe undefined id m | m <- ms, isJust m]
