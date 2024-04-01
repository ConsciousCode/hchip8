{-# LANGUAGE BlockArguments #-}

module BrickMain (run) where

import Brick ((<+>), (<=>), App(..), Padding(Pad), halt, customMain, neverShowCursor, hBox, vBox, hLimit, vLimit, hSize, padBottom, fill, AttrMap, attrMap, attrName, AttrName, withAttr, on, fg)
import Brick.Types (Widget, BrickEvent(..), EventM, put)
import Brick.Widgets.Border (borderWithLabel, hBorder, vBorder, border)
import Brick.Widgets.Core (str)
import Brick.Widgets.Table (table, renderTable)
import Brick.BChan (newBChan, writeBChan)
import Brick.AttrMap ()

import qualified Graphics.Vty as V
import qualified Graphics.Vty.CrossPlatform as VCP
import Graphics.Vty.Input.Events (Key(KEnter))

import Data.Time.Clock (UTCTime, getCurrentTime, diffUTCTime, nominalDiffTimeToSeconds)
import Data.Char (chr)
import Data.Bits ((.|.), bit)
import Data.Vector.Unboxed (toList, (!))

import Control.Concurrent (threadDelay, forkIO)
import Control.Monad (void, forever)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.State (modify, get, gets, when)

import Chip8 (Chip8, Emulator, pixel, width, height, emulate, countdown, rPC, rI, rV, delayT, soundT, readWord, dis, stack, latest)
import Util (rpad, hexPad, intersperse, join, vReg, m1)

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

renderScreen :: Chip8 -> String
renderScreen vm = concat [
    [braille (getb (x*2) (y*4))
              | x <- m1 [1..(width  vm `div` 2)]
    ] ++ "\n" | y <- m1 [1..(height vm `div` 4)]
  ]
  where getb x y ox oy = pixel vm (x + ox) (y + oy)

renderLabel :: AppState -> String
renderLabel as = "" -- show (xxx as)

drawV :: Chip8 -> Int -> Widget ()
drawV vm x = str $ vReg x ++ " " ++ hexPad 2 (v!x)
  where v = rV vm

drawVFile :: Chip8 -> Widget ()
drawVFile vm = borderWithLabel (str "VX")
  $ vLimit 8 $ rCol [0..7] <+> vBorder <+> rCol [8..0xF]
  where
    rCol = vBox . map (drawV vm)

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

drawStatus :: Chip8 -> Widget ()
drawStatus vm = vLimit 3 . border . hBox . intersperse vBorder . map str $ [
    "PC "    ++ hexPad 3 (rPC    vm),
    "I "     ++ hexPad 3 (rI     vm),
    "delay " ++ hexPad 2 (delayT vm),
    "sound " ++ hexPad 2 (soundT vm)
  ]

drawCode :: Chip8 -> Widget ()
drawCode vm = (border . str) content
  where
    pc = rPC vm
    content = hexPad 3 pc ++ " " ++ rpad " " 16 (dis (readWord pc vm))

-- Add a border as well so we can see an empty screen
drawScreen :: AppState -> Widget ()
drawScreen as = borderWithLabel label scr
  where
    vm = latest $ emulator as
    label = (str . renderLabel) as
    scr = (withAttr screenAttr . str . renderScreen) vm

draw :: AppState -> [Widget ()]
draw as = [drawScreen as <+> drawVFile vm <=> drawStatus vm <=> drawCode vm <+> drawStack vm]
  where vm = latest $ emulator as

data AppState = AppState {
  emulator  :: Emulator,   -- VM state
  lastFrame :: UTCTime  -- Last time a frame occurred
}

data ClockTick = Tick
-- Available events: BrickEvent in Brick.Types.
handleEvent :: BrickEvent () ClockTick -> EventM () AppState ()
handleEvent (AppEvent Tick) = do
  cur <- liftIO getCurrentTime
  lf  <- gets lastFrame
  let dt = nominalDiffTimeToSeconds (diffUTCTime cur lf)
  when (dt >= 1/60) do
    -- It took me like 12 hours to figure out why my countdown "wasn't running"
    --  - I was reusing vm which I got at the start, so it didn't reflect the
    --  updates in this `when` monad
    vm <- gets emulator
    modify $ \s -> s {
      lastFrame = cur,
      emulator = countdown vm -- Count down timers every frame
    }
  
  vm <- gets emulator
  modify $ \s -> s { emulator = emulate Nothing False vm }

-- So CTRL+C exits properly
handleEvent (VtyEvent (V.EvKey (V.KChar 'c') [V.MCtrl])) = halt

--handleEvent (VtyEvent (V.EvKey key [])) = case key of
--  KEnter -> do
    
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