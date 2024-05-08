module GlossMain (run) where

import Graphics.Gloss (Picture, play)
import Graphics.Gloss.Data.Color as Color (color)
import Graphics.Gloss.Data.Picture (rectangleSolid)
import Graphics.Gloss.Data.ViewPort (ViewPort)

import Data.Word (Word16)
import Control.Monad.State (execState)

import Chip8

import Util (filterJust)

data AppState = AppState {
  emulator :: Emulator,
  resTime  :: Float
}

data Args = Args {
  
}

render :: AppState -> Picture
render as = pictures $ filterJust [renderPx (x, y) | x <- [0..63], y <- [0..31]]
  where
    vm = current (emulator as)
    c = Color.black
    renderPx (x, y) = if pixel vm x y
      then Just $ translate x y $ color c $ rectangleSolid 1 1
      else Nothing

-- Now the last thing we need is to define the function to advance
-- the state
step :: ViewPort -> Float -> AppState -> AppState
step vp s = execState do
  emu <- gets emulator
  old <- gets resTime
  let res = old + s
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
handleEvent (EventKey (Char c) ks _ _ _) =
  case hexCosmac c of
    Nothing -> pure ()
    Just xk -> do
      emu <- gets emulator
      modify $ \s -> s {
        emulator = update (keyEvent xk (ks == Down)) emu
      }

-- Now we need just need to piece it all together
run :: Emulator -> IO ()
run args emu = do
  let
    fps = 60
    win = InWindow "CHIP-8" (640, 320) (0, 0)
    as  = AppState {
      emulator = emu,
      keys     = 0
    }
  play win black fps as render handleEvent step