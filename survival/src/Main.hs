{-# LANGUAGE OverloadedStrings #-}

module Main where

import qualified SDL
import qualified SDL.Image
import qualified Data.Set as Set
import Control.Monad (unless)
import Foreign.C.Types (CInt)

data GameState = GameState
  { playerPos :: (Float, Float)
  , activeKeys :: Set.Set SDL.Keycode
  , characterTexture :: SDL.Texture
  }

screenWidth, screenHeight :: CInt
screenWidth = 800
screenHeight = 600

main :: IO ()
main = do
  SDL.initialize [SDL.InitVideo]
  window <- SDL.createWindow "Survival" SDL.defaultWindow { SDL.windowInitialSize = SDL.V2 screenWidth screenHeight }
  renderer <- SDL.createRenderer window (-1) SDL.defaultRenderer
  
  characterSurface <- SDL.Image.load "../assets/rogues.bmp"

  characterTexture <- SDL.createTextureFromSurface renderer characterSurface
  SDL.freeSurface characterSurface

  let initialState = GameState { playerPos = (0, 0), activeKeys = Set.empty, characterTexture = characterTexture }
  
  gameLoop renderer initialState
  
  SDL.destroyTexture characterTexture
  SDL.destroyRenderer renderer
  SDL.destroyWindow window
  SDL.quit

gameLoop :: SDL.Renderer -> GameState -> IO ()
gameLoop renderer state = do
  events <- SDL.pollEvents
  let quit = any (eventIsQuit . SDL.eventPayload) events
      newState = foldl (flip handleEvent) state events
  
  SDL.rendererDrawColor renderer SDL.$= SDL.V4 255 255 255 255
  SDL.clear renderer
  render renderer newState
  SDL.present renderer
  
  unless quit $ do
    SDL.delay 16  -- Cap at roughly 60 FPS
    gameLoop renderer (update 0.016 newState)  -- 16ms ≈ 0.016s

eventIsQuit :: SDL.EventPayload -> Bool
eventIsQuit SDL.QuitEvent = True
eventIsQuit _ = False

handleEvent :: SDL.Event -> GameState -> GameState
handleEvent event game =
  case SDL.eventPayload event of
    SDL.KeyboardEvent keyboardEvent ->
      let keycode = SDL.keysymKeycode $ SDL.keyboardEventKeysym keyboardEvent
          keyState = SDL.keyboardEventKeyMotion keyboardEvent
      in case keyState of
           SDL.Pressed -> game { activeKeys = Set.insert keycode (activeKeys game) }
           SDL.Released -> game { activeKeys = Set.delete keycode (activeKeys game) }
    _ -> game

update :: Float -> GameState -> GameState
update dt game = game { playerPos = newPos }
  where
    (x, y) = playerPos game
    speed = 200 * dt
    dx = if SDL.KeycodeD `Set.member` activeKeys game then speed else 0 +
         if SDL.KeycodeA `Set.member` activeKeys game then -speed else 0
    dy = if SDL.KeycodeW `Set.member` activeKeys game then -speed else 0 +
         if SDL.KeycodeS `Set.member` activeKeys game then speed else 0

    newX = if x + dx > fromIntegral screenWidth / 2 then x + dx - fromIntegral screenWidth
           else if x + dx < -fromIntegral screenWidth / 2 then x + dx + fromIntegral screenWidth
           else x + dx
    newY = if y + dy > fromIntegral screenHeight / 2 then y + dy - fromIntegral screenHeight
           else if y + dy < -fromIntegral screenHeight / 2 then y + dy + fromIntegral screenHeight
           else y + dy
    newPos = (newX, newY)

render :: SDL.Renderer -> GameState -> IO ()
render renderer game = do
  let (x, y) = playerPos game
      destRect = SDL.Rectangle (SDL.P (SDL.V2 (round $ x + fromIntegral screenWidth / 2) (round $ y + fromIntegral screenHeight / 2))) (SDL.V2 64 64)
  SDL.copy renderer (characterTexture game) Nothing (Just destRect)




