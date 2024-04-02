{-# LANGUAGE BlockArguments #-}

module BrickMain (run) where

import Brick ((<+>), (<=>), App(..), Padding(Pad, Max), padRight, halt, customMain, neverShowCursor, hBox, vBox, hLimit, vLimit, hSize, padBottom, fill, AttrMap, attrMap, attrName, AttrName, withAttr, on, fg, setAvailableSize)
import Brick.Types (Widget, BrickEvent(..), EventM, put)
import Brick.Widgets.Border (borderWithLabel, hBorder, vBorder, border)
import Brick.Widgets.Core (str)
import Brick.Widgets.Table (table, renderTable)
import Brick.BChan (newBChan, writeBChan)
import Brick.AttrMap ()

import qualified Graphics.Vty as V
import qualified Graphics.Vty.CrossPlatform as VCP
import Graphics.Vty.Input.Events (Key(KChar, KEnter, KLeft, KRight))

import Data.Time.Clock (UTCTime, getCurrentTime, diffUTCTime, nominalDiffTimeToSeconds)
import Data.Char (chr)
import Data.Bits ((.|.), bit)
import Data.List (elemIndex)
import Data.Vector.Unboxed (toList, (!))

import Control.Concurrent (threadDelay, forkIO)
import Control.Monad (void, forever)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.State (modify, get, gets, when)

import Chip8 (Chip8, Emulator, pixel, width, height, emulate, countdown, rPC, rI, rV, delayT, soundT, readWord, dis, stack, latest, update, states, stPos, togglePause, forward, rewind, current, keyEvent)
import Util (lpad, rpad, hexPad, intersperse, join, vReg, m1, PColor)

screenAttr :: AttrName
screenAttr = attrName "screenAttr"

curInsAttr :: AttrName
curInsAttr = attrName "curInsAttr"

brInsAttr :: AttrName
brInsAttr = attrName "brInsAttr"

-- Construct a function which takes a function for getting bits and returns
--  a character representing some block of pixels
charFrom :: String -> [[Int]] -> (Int -> Int -> Bool) -> Char
charFrom cs ps gb = cs!!
  foldl (.|.) 0 [
    if gb x y then bit b else 0
    | (y, row) <- zip [0..] ps
    , (x, b  ) <- zip [0..] row
  ]

{-
This project most readily adapts to Gloss, but I had a cool idea for how to
 adapt it to Brick as well. Unicode has a braille block which supports
 extended 2x4 braille for a total of 256 combinations. Effectively, this allows
 us to represent 2x4 boolean pixels with a single braille character provided
 the terminal has a size of at least 32x8 (not hard to get).

This was the first one I implemented, then generalized into charFrom
 -}
charBraille :: (Int -> Int -> Bool) -> Char
charBraille = charFrom ['\10240'..'\10495'] bpos
  where -- The braille block is ordered like 76543210:
    bpos = [
      [0, 3],
      [1, 4],
      [2, 5],
      [6, 7]]

{-
Once I implemented using braille, I remembered box drawing and generalized.
 Braille is still actually the most compact form, because there's no 2x4
 box drawing (only 1x1, 1x2, and 2x3), but it's also a tad ugly.
 -}

charAscii :: (Int -> Int -> Bool) -> Char
charAscii = charFrom " #" [[0]]

char1x1 :: (Int -> Int -> Bool) -> Char
char1x1 = charFrom " █" [[0]]

char1x2 :: (Int -> Int -> Bool) -> Char
char1x2 = charFrom " ▀▄█" [[0], [1]]

char2x2 :: (Int -> Int -> Bool) -> Char
char2x2 = charFrom " ▘▝▀▖▌▞▛▗▚▐▜▄▙▟█" [[0, 1], [2, 3]]

char2x3 :: (Int -> Int -> Bool) -> Char
char2x3 = charFrom cs [[0, 1], [2, 3], [4, 5]]
  where
    cs =
      " " ++ ['\129792'..'\129811'] ++
      "▌" ++ ['\129812'..'\129831'] ++
      "▐" ++ ['\129832'..'\129851'] ++
      "█"

-- Render the screen using a pixel block rendering function
renderScreen :: ((Int -> Int -> Bool) -> Char) -> Int -> Int -> Chip8 -> String
renderScreen cf w h vm = concat [
    [cf (getb (x*w) (y*h))
              | x <- m1 [1..(width  vm `div` w)]
    ] ++ "\n" | y <- m1 [1..(height vm `div` h)]
  ]
  where getb x y ox oy = pixel vm (x + ox) (y + oy)

-- Placeholder for what to put on the screen's border
renderLabel :: AppState -> String
renderLabel as = "" -- show (xxx as)

-- Draw a widget for a register
drawV :: Chip8 -> Int -> Widget ()
drawV vm x = str $ vReg x ++ " " ++ hexPad 2 (v!x)
  where v = rV vm

-- Draw all the registers
drawVFile :: Chip8 -> Widget ()
drawVFile vm = borderWithLabel (str "VX")
  $ vLimit 8 $ rCol [0..7] <+> vBorder <+> rCol [8..0xF]
  where
    rCol = vBox . map (drawV vm)

-- Draw the call stack
drawStack :: Chip8 -> Widget ()
drawStack vm = build (stack vm)
  where
    ms = 12
    pad [] = pad [str "   "]
    pad xs = (padBottom (Pad (ms - length xs)) . vBox) xs
    build sp =
      (borderWithLabel (str "SP") .
      hLimit 3 . vLimit 12 . vBox)
      (((map (str . hexPad 3) . reverse) sp) ++ [fill ' '])

-- Draw the VM status
drawStatus :: Chip8 -> Widget ()
drawStatus vm = vLimit 3 . border . hBox . intersperse vBorder . map str $ [
    "PC "    ++ hexPad 3 (rPC    vm),
    "I "     ++ hexPad 3 (rI     vm),
    "delay " ++ hexPad 2 (delayT vm),
    "sound " ++ hexPad 2 (soundT vm)
  ]

-- Draw one opcode
drawCode :: Chip8 -> Int -> Widget ()
drawCode vm pc = padRight Max $ str $ hexPad 3 pc ++ " " ++ rpad " " 16 (dis (readWord pc vm))

-- Recursively draw the last 5 opcodes
drawCodes :: [Chip8] -> Int -> [Widget ()] -> [Widget ()]
drawCodes [] _ acc = fill ' ':acc -- Ran out of states
drawCodes (vm:vms) old acc
  -- We're done
  | length acc >= 5 = acc
  -- The recorded state didn't change the PC, ignore it
  | old == new      = nextRender acc
  -- Result of a branch, add a border to indicate discontinuity
  | old - new /= 2  = nextRender (withAttr brInsAttr code:acc)
  -- Consecutive opcodes
  | otherwise       = nextRender (code:acc)
  where
    nextRender = drawCodes vms new
    new  = fromIntegral (rPC vm)
    code = drawCode vm new

-- Wraps drawCodes in a box
drawCodesBox :: Emulator -> Widget ()
drawCodesBox emu = (border . setAvailableSize (16, 5) . vBox) codes
  where
    ss = states emu
    st = stPos emu
    vm = ss!!st
    pc = fromIntegral (rPC vm)
    codes = drawCodes (drop st ss) pc [withAttr curInsAttr $ drawCode vm pc]

-- Add a border as well so we can see an empty screen
drawScreen :: AppState -> Widget ()
drawScreen as = borderWithLabel label scr
  where
    rf    = render as
    vm    = current $ emulator as
    label = (str . renderLabel) as
    scr   = (withAttr screenAttr . str . rf) vm

-- Draw the whole program
draw :: AppState -> [Widget ()]
draw as = [drawScreen as <+> drawVFile vm <=> drawStatus vm <=> drawCodesBox emu <+> drawStack vm]
  where
    emu = emulator as
    vm = current emu

-- Total state of the app
data AppState = AppState {
  -- How to render the screen
  render    :: Chip8 -> String,
  emulator  :: Emulator, -- VM state
  lastFrame :: UTCTime   -- Last time a frame occurred
}

-- Update the emulator with a mutation function
updateEmu :: (Emulator -> Emulator) -> AppState -> AppState
updateEmu todo as = as { emulator = todo (emulator as) }

data ClockTick = Tick
-- Available events: BrickEvent in Brick.Types.
handleEvent :: BrickEvent () ClockTick -> EventM () AppState ()
handleEvent (AppEvent Tick) = do
  cur <- liftIO getCurrentTime
  lf  <- gets lastFrame
  let dt = nominalDiffTimeToSeconds (diffUTCTime cur lf)
  when (dt >= 1/60) do
    modify $ \s -> s {
      emulator  = update countdown (emulator s),
      lastFrame = cur -- Countdown every frame
    }
  
  modify $ updateEmu emulate

-- So CTRL+C exits properly
handleEvent (VtyEvent (V.EvKey (V.KChar 'c') [V.MCtrl])) = halt

handleEvent (VtyEvent (V.EvKey key [])) = do
  modify $ updateEmu case key of
    KEnter  -> togglePause
    KLeft   -> rewind
    KRight  -> forward
    KChar c
      | c `elem` kp -> case c `elemIndex` kp of
        Nothing -> id
        Just k  -> update $ keyEvent k True
    _ -> id
    where -- Chip-8 hex keypad mapping
      kp =
        "1234\
        \QWER\
        \ASDF\
        \ZXCV"

-- Any other event just do nothing
handleEvent _ = return ()

-- Now lets create our App
theMap :: AttrMap
theMap = attrMap V.defAttr
  [
    (screenAttr, V.yellow `on` V.color240 127 64 0),
    (curInsAttr, V.defAttr `V.withStyle` V.reverseVideo),
    (brInsAttr, V.defAttr `V.withStyle` V.underline)
  ]

app :: App AppState ClockTick ()
app = App
  { appDraw         = draw
  , appChooseCursor = neverShowCursor
  , appHandleEvent  = handleEvent
  , appStartEvent   = pure ()
  , appAttrMap      = const theMap
  }

-- Now we can finally pull it all together
run :: PColor -> PColor -> Emulator -> IO ()
run fg bg emu = do
  let delay = 1000 -- 1 μs ~ 1 MHz
  chan <- newBChan 10
  
  void . forkIO $ forever $ do
    writeBChan chan Tick
    threadDelay delay
  
  let builder = VCP.mkVty V.defaultConfig
  let rf = renderScreen char2x3 2 3
  cur <- liftIO getCurrentTime
  let as = AppState rf emu cur
  initialVty <- builder
  _ <- customMain initialVty builder (Just chan) app as
  return ()