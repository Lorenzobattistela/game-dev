{-# LANGUAGE OverloadedStrings #-}

module Main where

import qualified SDL
import qualified SDL.Image
import qualified SDL.Font as TTF
import qualified Data.Map as Map
import Control.Monad (unless)
import Control.Monad.IO.Class (MonadIO)
import Data.Word (Word32)
import Foreign.C.Types (CInt)
import Data.IORef
import Data.Map (Map)
import System.Exit (exitSuccess)

import Game
import Rendering

-- in the C version, a lot of things are "flying" in the global state
-- This is not good in haskell, because its not a loop and i cant update the globals
-- So its probably best to embed all of it in super "GameState" structure and return it for every 'game interaction'
-- a game state should have the following fields
-- an array / vector of enemies
-- initial player
-- probably a keyboard state to handle key presses
-- these frameTime data to smooth the animation maybe?

type ControlMap = Map Controls Bool

initControlMap :: ControlMap
initControlMap = Map.fromList [(c, False) | c <- [W .. D]]

initEnemies :: [Enemy]
initEnemies =
  let numEnemies = 1
      life = 20
      damage = 10
      speed = fromIntegral 2
  in createEnemies numEnemies life damage speed

initPlayer :: Player
initPlayer = Player 
  { pPosition = Position { x = 0, y = 0 } 
  , life = 20
  , pSpeed = fromIntegral 3
  , pDead = False
  , pCurrentFrame = FRONT_STANDING
  }

updateControl :: Controls -> Bool -> ControlMap -> ControlMap
updateControl control value = Map.insert control value

getControl :: Controls -> ControlMap -> Maybe Bool
getControl = Map.lookup 

data GameState = GameState
  { controlMap :: ControlMap
  , enemies    :: [Enemy]
  , player     :: Player
  , loopState :: LoopState
  , renderer :: SDL.Renderer
  } deriving (Eq, Show)

initialGameState :: SDL.Renderer -> GameState
initialGameState renderer = GameState
  { controlMap = initControlMap
  , enemies = initEnemies
  , player = initPlayer
  , loopState = Running
  , renderer = renderer
  }

gameLoop :: SDL.Window -> IO ()
gameLoop window = do
  let loop state = do
    frameStart <- SDL.ticks
    events <- SDL.pollEvents
    newState <- handleEvents state events

    unless (newState == Quit) $ do
      SDL.glSwapWindow window
      loop newState
    loop Running 

handleEvents :: MonadIO m => GameState -> [SDL.Event] -> m GameState
handleEvents state events = do
  foldr handleEvent (return state) events

handleEvent :: MonadIO m => SDL.Event -> m GameState -> m GameState
handleEvent event stateM = do
  state <- stateM
  case SDL.eventPayload event of
    SDL.QuitEvent -> return Quit
    SDL.KeyboardEvent keyboardEvent ->
      if SDL.KeyboardEventKeyMotion keyboardEvent === SDL.Pressed
      then handleKeyPress (SDL.keysymKeycode $ SDL.keyboardEventKeysym keyboardEvent) state
      else handleKeyRelease (SDL.keysymKeycode $ SDL.keyboardEventKeysym keyboardEvent) state 
    _ -> return state


main :: IO ()
main = do
  SDL.initialize [SDL.InitVideo]
  TTF.initialize

  window <- SDL.createWindow "Survival" SDL.defaultWindow { SDL.windowInitialSize = SDL.V2 windowWidth windowHeight }
  renderer <- SDL.createRenderer window (-1) SDL.defaultRenderer

  gameState <- newIORef $ initialGameState renderer

  return ()

