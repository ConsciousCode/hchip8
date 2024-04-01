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
import Graphics.Vty.Input.Events (Key(KEnter, KLeft, KRight))

import Data.Time.Clock (UTCTime, getCurrentTime, diffUTCTime, nominalDiffTimeToSeconds)
import Data.Char (chr)
import Data.Bits ((.|.), bit)
import Data.Vector.Unboxed (toList, (!))

import Control.Concurrent (threadDelay, forkIO)
import Control.Monad (void, forever)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.State (modify, get, gets, when)

import Chip8 (Chip8, Emulator, pixel, width, height, emulate, countdown, rPC, rI, rV, delayT, soundT, readWord, dis, stack, latest, update, states, stPos, togglePause, forward, rewind)
import Util (lpad, rpad, hexPad, intersperse, join, vReg, m1)

screenAttr :: AttrName
screenAttr = attrName "screenAttr"

{-
This project most readily adapts to Gloss, but I had a cool idea for how to
 adapt it to Brick as well. Unicode has a braille block which supports
 extended 2x4 braille for a total of 256 combinations. Effectively, this allows
 us to represent 2x4 boolean pixels with a single braille character provided
 the terminal has a size of at least 32x8 (not hard to get).
 -}

braille :: (Int -> Int -> Bool) -> Char
braille gb = chr $ foldl (.|.) 0x2800 [
    if gb x y then bit b else 0
    | (y, row) <- zip [0..] bpos
    , (x, b  ) <- zip [0..] row
  ]
  where -- The braille block is ordered like 76543210:
    bpos = [
      [0, 3],
      [1, 4],
      [2, 5],
      [6, 7]]

-- Render the screen as a 32x8 grid of braille characters
renderScreen :: Chip8 -> String
renderScreen vm = concat [
    [braille (getb (x*2) (y*4))
              | x <- m1 [1..(width  vm `div` 2)]
    ] ++ "\n" | y <- m1 [1..(height vm `div` 4)]
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
  | old - new /= 2  = if length acc == 4
    then hBorder:acc -- Make sure we don't go over
    else nextRender (code:hBorder:acc)
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
    st = drop (stPos emu) (states emu)
    pc = fromIntegral (rPC vm)
    vm = latest emu
    codes = drawCodes st pc [drawCode vm pc]

-- Add a border as well so we can see an empty screen
drawScreen :: AppState -> Widget ()
drawScreen as = borderWithLabel label scr
  where
    vm = latest $ emulator as
    label = (str . renderLabel) as
    scr = (withAttr screenAttr . str . renderScreen) vm

-- Draw the whole program
draw :: AppState -> [Widget ()]
draw as = [drawScreen as <+> drawVFile vm <=> drawStatus vm <=> drawCodesBox emu <+> drawStack vm]
  where
    emu = emulator as
    vm = latest emu

-- Total state of the app
data AppState = AppState {
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
  
  modify $ \s -> s { emulator = emulate (emulator s) }

-- So CTRL+C exits properly
handleEvent (VtyEvent (V.EvKey (V.KChar 'c') [V.MCtrl])) = halt

handleEvent (VtyEvent (V.EvKey key [])) = case key of
  KEnter -> do
    modify $ updateEmu togglePause
  KLeft  -> do
    modify $ updateEmu rewind
  KRight -> do
    modify $ updateEmu forward

--handleEvent (VtyEvent (V.EvKey (V.KChar ' ') [])) = do
--  modify $ \s -> s { }

-- Any other event just do nothing
handleEvent _ = return ()

-- Now lets create our App
theMap :: AttrMap
theMap = attrMap V.defAttr
  [
    (screenAttr, fg V.white `V.withStyle` V.bold)
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
run :: Emulator -> IO ()
run emu = do
  let delay = 10000 -- 1 μs ~ 1 MHz
  chan <- newBChan 10
  
  void . forkIO $ forever $ do
    writeBChan chan Tick
    threadDelay delay
  
  let builder = VCP.mkVty V.defaultConfig
  cur <- liftIO getCurrentTime
  let as = AppState emu cur
  initialVty <- builder
  _ <- customMain initialVty builder (Just chan) app as
  return ()