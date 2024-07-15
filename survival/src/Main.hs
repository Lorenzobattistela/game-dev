{-# LANGUAGE OverloadedStrings #-}

module Main where

import qualified SDL
import qualified SDL.Image
import qualified SDL.Font as TTF
import Control.Monad (unless)
import Foreign.C.Types (CInt)
import Data.IORef
import System.Exit (exitSuccess)

import Game
import Rendering
  
main :: IO ()
main = do
  SDL.initialize [SDL.InitVideo]
  TTF.initialize

  window <- SDL.createWindow "Survival" SDL.defaultWindow { SDL.windowInitialSize = SDL.V2 windowWidth windowHeight }
  renderer <- SDL.createRenderer window (-1) SDL.defaultRenderer

  return ()
