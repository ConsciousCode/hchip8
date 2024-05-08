{-# LANGUAGE BlockArguments #-}

module BrickMain (run, Args(Args, baBPs, baRes, baFg, baBg)) where

import Brick ((<+>), (<=>), App(..), Padding(Max), padRight, halt, customMain, neverShowCursor, hBox, vBox, hLimit, vLimit, fill, attrMap, attrName, AttrName, withAttr, on, setAvailableSize)
import Brick.Types (Widget, BrickEvent(..), EventM)
import Brick.Widgets.Border (borderWithLabel, vBorder, border)
import Brick.Widgets.Core (str)
import Brick.Widgets.Table (table, renderTable, rowBorders, columnBorders)
import Brick.BChan (newBChan, writeBChan)

import qualified Graphics.Vty as V
import qualified Graphics.Vty.CrossPlatform as VCP
import Graphics.Vty.Input.Events (Key(KChar, KEnter, KLeft, KRight))

import Data.Time.Clock (UTCTime, getCurrentTime, diffUTCTime, nominalDiffTimeToSeconds)
import Data.Char (toLower)
import Data.Bits ((.|.), bit, testBit)
import Data.Vector.Unboxed (Vector, generate, (!), (//))

import Control.Concurrent (threadDelay, forkIO)
import Control.Monad (void, forever)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.State (modify, gets, when)

import Chip8 (Chip8, Emulator, pixel, width, height, emulate, countdown, rPC, rI, rV, delayT, soundT, readWord, dis, stack, latest, update, states, stPos, togglePause, forward, rewind, current, keyEvent, keys, cycles, frames)
import Util (cosmac, rpad, hexPad, hexDigit, hexInt, intersperse, vReg, m1, unfold, hexCosmac)

type Vec = Vector

keyClear :: Int
keyClear = 33

screenAttr :: AttrName
screenAttr = attrName "screenAttr"

curInsAttr :: AttrName
curInsAttr = attrName "curInsAttr"

brInsAttr :: AttrName
brInsAttr = attrName "brInsAttr"

keySelAttr :: AttrName
keySelAttr = attrName "keySelAttr"

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

charAscii1x1 :: (Int -> Int -> Bool) -> Char
charAscii1x1 = charFrom " #" [[0]]

charAscii1x2 :: (Int -> Int -> Bool) -> Char
charAscii1x2 = charFrom " '.:" [[0], [1]]

charAscii2x2 :: (Int -> Int -> Bool) -> Char
charAscii2x2 = charFrom " `'^,;/r.\\|7_LJ#" [[0, 1], [2, 3]]

char1x1 :: (Int -> Int -> Bool) -> Char
char1x1 = charFrom " █" [[0]]

char1x2 :: (Int -> Int -> Bool) -> Char
char1x2 = charFrom " ▀▄█" [[0], [1]]

char1x8 :: (Int -> Int -> Bool) -> Char
char1x8 = charFrom "🌕🌖🌗🌘🌑🌒🌓🌔" [[0..8]]

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
    [cf (getb (x*w) (y*h)) -- + w/h - 1 to round up
              | x <- m1 [1..((width  vm + w - 1) `div` w)]
    ] ++ "\n" | y <- m1 [1..((height vm + h - 1) `div` h)]
  ]
  where getb x y ox oy = pixel vm (x + ox) (y + oy)

-- Placeholder for what to put on the screen's border
renderLabel :: AppState -> String
renderLabel _ = "" -- show (xxx as)

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
    build sp =
      (borderWithLabel (str "SP") .
      hLimit 3 . vLimit 12 . vBox)
      (((map (str . hexPad 3) . reverse) sp) ++ [fill ' '])

-- Draw the VM status
drawStatus :: Chip8 -> Widget ()
drawStatus vm = ss
  where
    ss = vLimit 3 . border . hBox . intersperse vBorder . map str $ [
      "I "      ++ hexPad 3 (rI     vm),
      "delay "  ++ hexPad 2 (delayT vm),
      "sound "  ++ hexPad 2 (soundT vm),
      "cycles " ++ show     (cycles vm),
      "frames " ++ show     (frames vm)]

-- Draw keys and their selections
drawKeypad :: Chip8 -> Widget ()
drawKeypad vm = renderTable
  . rowBorders False . columnBorders False . table
  . reverse . unfold (not . null) (map go . take 4) (drop 4) $ cosmac
  where
    go k
      | keys vm `testBit` xk = withAttr keySelAttr key
      | otherwise = key
      where
        key = str [k]
        (Just xk) = hexDigit k

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
draw as = [drawScreen as <+> drawVFile vm <=> drawStatus vm <=> (drawCodesBox emu <+> drawKeypad vm) <+> drawStack vm]
  where
    emu = emulator as
    vm = current emu

-- Total state of the apppress a key, there's a short delay
data AppState = AppState {
  -- How to render the screen
  render    :: Chip8 -> String,
  keyTimes  :: Vec Int,  -- List of cycles mod 60 to emulate key releases
  emulator  :: Emulator, -- VM state
  lastFrame :: UTCTime   -- Last time a frame occurred
}

data Args = Args {
  baBPs :: [Int],  -- List of breakpoints
  baRes :: String, -- Resolution type
  baFg  :: String, -- Foreground color
  baBg  :: String  -- Background color
}
  deriving (Show)

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
    kt <- gets keyTimes
    fs <- gets (frames . current . emulator)
    mapM_ (modify . updateEmu . update) [
      keyEvent i False
      | i <- [0..15],
        fs - kt!i > keyClear
      ]
    modify $ \s -> s {
      emulator  = update countdown (emulator s),
      lastFrame = cur -- Countdown every frame
    }
  
  modify $ updateEmu emulate

-- So CTRL+C exits properly
handleEvent (VtyEvent (V.EvKey (V.KChar 'c') [V.MCtrl])) = halt

handleEvent (VtyEvent (V.EvKey key [])) = do
  case key of
    KEnter  -> modify $ updateEmu togglePause
    KLeft   -> modify $ updateEmu rewind
    KRight  -> modify $ updateEmu forward
    KChar c -> case hexCosmac c of
      Nothing -> pure ()
      Just xk -> do
        emu <- gets emulator
        modify $ \s -> s {
          emulator = update (keyEvent xk True) emu,
          keyTimes = keyTimes s//[(xk, frames (latest emu))]
        }
          
    _ -> pure ()

-- Any other event just do nothing
handleEvent _ = return ()

strColor :: String -> Maybe V.Color
strColor c = case map toLower c of
  ""        -> Nothing -- So head later won't panic
  "red"     -> Just V.red
  "blue"    -> Just V.blue
  "green"   -> Just V.green
  "yellow"  -> Just V.yellow
  "white"   -> Just V.white
  "black"   -> Just V.black
  "magenta" -> Just V.magenta
  "cyan"    -> Just V.cyan
  _
    | head c == '#' -> case length c of
      4 -> case hexInt (tail c) of
        Nothing -> Nothing -- not hex
        Just c3 -> Just $ V.color240 (r*0x11) (g*0x11) (b*0x11) -- x -> xx
          where
            (c2, b) = c3 `divMod` 16
            (r,  g) = c2 `divMod` 16
      7 -> case hexInt (tail c) of
        Nothing -> Nothing -- not hex
        Just c6 -> Just $ V.color240 r g b
          where
            (c4, b) = c6 `divMod` 256
            (r,  g) = c4 `divMod` 256
      _ -> Nothing
    | otherwise -> Nothing

strRenderType :: String -> (Chip8 -> String)
strRenderType srt = case map toLower srt of
  "ascii1x1" -> renderScreen charAscii1x1 1 1
  "ascii1x2" -> renderScreen charAscii1x2 1 2
  "ascii2x2" -> renderScreen charAscii2x2 2 2
  "braille"  -> renderScreen charBraille  2 4
  "1x1"      -> renderScreen char1x1      1 1
  "1x2"      -> renderScreen char1x2      1 2
  "1x8"      -> renderScreen char1x8      1 8
  "moon"     -> renderScreen char1x8      1 8
  "2x2"      -> renderScreen char2x2      2 2
  "2x3"      -> renderScreen char2x3      2 3
  _          -> strRenderType "2x3" -- default

-- Now we can finally pull it all together
run :: Args -> Emulator -> IO ()
run args emu = do
  cur <- liftIO getCurrentTime
  let
    delay = 1000 -- 1 ms ~ 1 kHz
    rf = strRenderType (baRes args)
    fc = maybe V.white id $ strColor (baFg args)
    bc = maybe V.black id $ strColor (baBg args)
    
    as = AppState rf (generate 16 (const 0)) emu cur
    app = App {
      appDraw         = draw,
      appChooseCursor = neverShowCursor,
      appHandleEvent  = handleEvent,
      appStartEvent   = pure (),
      appAttrMap      = const $ attrMap V.defAttr [
        (screenAttr, fc `on` bc),
        (curInsAttr, V.defAttr `V.withStyle` V.reverseVideo),
        (brInsAttr , V.defAttr `V.withStyle` V.underline),
        (keySelAttr, V.defAttr `V.withStyle` V.reverseVideo)
      ]
    }
    builder = VCP.mkVty V.defaultConfig
  
  chan <- newBChan 10
  void . forkIO $ forever $ do
    writeBChan chan Tick
    threadDelay delay
  
  initialVty <- builder
  _ <- customMain initialVty builder (Just chan) app as
  return ()