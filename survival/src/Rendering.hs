module Rendering where

import qualified SDL
import qualified SDL.Image
import Control.Monad (forM_)
import Foreign.C.String (withCString)
import Foreign.C.Types (CInt)
import Data.Vector (Vector)
import qualified Data.Vector as V


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















