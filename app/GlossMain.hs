{-# LANGUAGE BlockArguments #-}

module GlossMain (run) where

import Graphics.Gloss (Picture, play, black, bitmapOfByteString, BitmapFormat(BitmapFormat), Display(InWindow))
import qualified Graphics.Gloss.Interface.Pure.Game as G
import Graphics.Gloss.Interface.Pure.Game (Event(EventKey), KeyState(Down))
import Graphics.Gloss.Data.Picture (rectangleSolid)
import Graphics.Gloss.Data.Bitmap (RowOrder(TopToBottom), PixelFormat(PxRGBA))
import Graphics.Gloss.Data.ViewPort (ViewPort)

import Data.Char (toLower)
import Data.Word (Word8, Word16, Word32)
import Data.Bits (shiftR, (.&.))
import Data.ByteString (ByteString, pack)
import Control.Monad.State (execState, gets, modify)

import Chip8

import Util (filterJust, hexInt, hexCosmac)

data AppState = AppState {
  emulator :: Emulator,
  resTime  :: Float,
  fgColor  :: Word32,
  bgColor  :: Word32
}

data Args = Args {
  gaFg :: String,
  gaBg :: String
}

unpackRGBA :: Word32 -> [Word8]
unpackRGBA w = [w2, w1, w0, 0xff]
  where
    w0 = fromIntegral (w             .&. 0xff) :: Word8
    w1 = fromIntegral (w `shiftR` 8  .&. 0xff) :: Word8
    w2 = fromIntegral (w `shiftR` 16 .&. 0xff) :: Word8

unpackPxWord :: [Word32] -> ByteString
unpackPxWord = pack . concat . map unpackRGBA

render :: AppState -> Picture
render as = bitmapOfByteString 64 32 (BitmapFormat TopToBottom PxRGBA) px False
  where
    fg = fgColor as
    bg = bgColor as
    vm = current (emulator as)
    px = unpackPxWord [if pixel vm x y then fg else bg | x <- [0..63], y <- [0..31]]

-- Now the last thing we need is to define the function to advance
-- the state
step :: ViewPort -> Float -> AppState -> AppState
step vp sec = execState do
  emu <- gets emulator
  old <- gets resTime
  let res = old + sec
  modify $ \s -> 
    if res >= 1/60 then 
      s {
        emulator = update countdown emu,
        resTime  = res - 1/60
      }
    else
      s {
        emulator = emulate emu
      }

handleEvent :: Event -> AppState -> AppState
handleEvent (EventKey (G.Char c) ks _ _) = execState
  case hexCosmac c of
    Nothing -> pure ()
    Just xk -> do
      emu <- gets emulator
      modify $ \s -> s {
        emulator = update (keyEvent xk (ks == Down)) emu
      }

strColor :: String -> Maybe Word32
strColor c = case map toLower c of
  ""        -> Nothing -- So head later won't panic
  "red"     -> Just 0xff0000
  "blue"    -> Just 0x0000ff
  "green"   -> Just 0x00ff00
  "yellow"  -> Just 0xffff00
  "white"   -> Just 0xffffff
  "black"   -> Just 0x000000
  "magenta" -> Just 0xff00ff
  "cyan"    -> Just 0x00ffff
  _
    | head c == '#' -> case length c of
      4 -> case hexInt (tail c) of
        Nothing -> Nothing -- not hex
        Just c3 -> Just (fromIntegral (r*0x11 + g*0x11 + b*0x11) :: Word32) -- x -> xx
          where
            (c2, b) = c3 `divMod` 16
            (r,  g) = c2 `divMod` 16
      7 -> hexInt (tail c) >>= \x -> Just (fromIntegral x :: Word32)
      _ -> Nothing
    | otherwise -> Nothing

-- Now we need just need to piece it all together
run :: Args -> Emulator -> IO ()
run args emu = do
  let
    fps = 60
    win = InWindow "CHIP-8" (640, 320) (0, 0)
    as  = AppState {
      emulator = emu,
      fgColor  = maybe 0xffffff id (strColor (gaFg args)),
      bgColor  = maybe 0x000000 id (strColor (gaBg args))
    }
  play win black fps as render handleEvent step