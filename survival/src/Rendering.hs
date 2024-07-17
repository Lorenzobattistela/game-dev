module Rendering where

import qualified SDL
import qualified SDL.Image
import qualified SDL.Font as TTF
import qualified SDL.Video.Renderer as Renderer
import Control.Monad (forM_, when)
import Control.Exception (bracket)
import Foreign.C.String (withCString)
import Foreign.C.Types (CInt)
import Data.Vector (Vector)
import qualified Data.Vector as V
import Game


-- since were receiving everything as args no need to a loadEnemySpritesheet fn as in C
loadSpritesheet :: SDL.Renderer -> FilePath -> Int -> Int -> Int -> Int -> IO (SDL.Texture, Vector (SDL.Rectangle CInt))
loadSpritesheet renderer path animationFrames frameCols spriteWidth spriteHeight = do
    surface <- SDL.Image.load path
    texture <- SDL.createTextureFromSurface renderer surface
    SDL.freeSurface surface

    let spriteClips = V.generate animationFrames $ \i -> 
            let row = i `div` frameCols
                col = i `mod` frameCols
            in SDL.Rectangle
                (SDL.P (SDL.V2 (fromIntegral $ col * spriteWidth)
                           (fromIntegral $ row * spriteHeight)))
                (SDL.V2 (fromIntegral spriteWidth) (fromIntegral spriteHeight))

    return (texture, spriteClips)

renderCharacter :: SDL.Renderer 
                -> (SDL.Texture, Vector (SDL.Rectangle CInt))  -- This is what loadSpritesheet returns
                -> Int  -- x position
                -> Int  -- y position
                -> Int  -- Current frame
                -> Int  -- sprite width
                -> Int  -- sprite height
                -> IO ()
renderCharacter renderer (spritesheet, spriteClips) x y currentFrame spriteWidth spriteHeight = do
    let renderQuad = SDL.Rectangle 
            (SDL.P (SDL.V2 (fromIntegral x) (fromIntegral y)))
            (SDL.V2 (fromIntegral spriteWidth) (fromIntegral spriteHeight))
    
    let sourceRect = spriteClips V.! currentFrame
    SDL.copy renderer spritesheet (Just sourceRect) (Just renderQuad)


renderEnemy :: SDL.Renderer -> (SDL.Texture, Vector (SDL.Rectangle CInt)) 
               -> Enemy -> Int -> Int -> IO ()
renderEnemy renderer (spritesheet, enemyClips) enemy enemyWidth enemyHeight = do
  let renderQuad = SDL.Rectangle
          (SDL.P (SDL.V2 ( round $ x (ePosition enemy)) (round $ y (ePosition enemy))))
          (SDL.V2 (fromIntegral enemyWidth) (fromIntegral enemyHeight))
  
  let sourceRect = enemyClips V.! (fromEnum (currentFrame enemy))
  SDL.copy renderer spritesheet (Just sourceRect) (Just renderQuad)


-- render all enemies if enemy is not dead. pass enemyWidth = 36 and enemyHeight = 36 to renderEnemy
renderEnemies :: SDL.Renderer -> (SDL.Texture, Vector (SDL.Rectangle CInt)) -> [Enemy] -> IO ()
renderEnemies renderer enemySpritesheet enemies = do
  forM_ enemies $ \enemy ->
    when (not $ eDead enemy) $
      renderEnemy renderer enemySpritesheet enemy 36 36

renderFloor :: SDL.Renderer -> FilePath -> IO ()
renderFloor renderer path = do
  let tileWidth = 45
      tileHeight = 20
      windowWidth = 950
      windowHeight = 800

  surface <- SDL.Image.load path
  texture <- SDL.createTextureFromSurface renderer surface
  SDL.freeSurface surface

  let srcRect = SDL.Rectangle (SDL.P (SDL.V2 16 200)) (SDL.V2 45 20)  
      windowSize = SDL.V2 windowWidth windowHeight
      tilesX = (windowWidth + tileWidth - 1) `div` tileWidth
      tilesY = (windowHeight + tileHeight - 1) `div` tileHeight

  forM_ [0..tilesY-1] $ \y ->
    forM_ [0..tilesX-1] $ \x -> do
      let dstRect = SDL.Rectangle
              (SDL.P (SDL.V2 (fromIntegral (x * tileWidth)) (fromIntegral (y * tileHeight))))
              (SDL.V2 (fromIntegral tileWidth) (fromIntegral tileHeight))
      SDL.copy renderer texture (Just srcRect) (Just dstRect)
  SDL.destroyTexture texture










