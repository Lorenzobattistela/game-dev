module Main where

import Graphics.Gloss
import Graphics.Gloss.Data.ViewPort
import Graphics.Gloss.Interface.Pure.Game

width, height :: Int
width = 600
height = 600

data PongGame = Game
  { ballLoc :: (Float, Float) -- x y location for pong ball
  , ballVeloc :: (Float, Float) -- pong ball velocity
  , player1 :: Float -- left player paddle height where 0 is middle screen
  , player2 :: Float --right player paddle height
  } deriving Show

initialState :: PongGame
initialState = Game
  { ballLoc = (-10, 30)
  , ballVeloc = (30, -2)
  , player1 = 40
  , player2 = -80 
  }

render :: PongGame -> Picture
render game =
  pictures [ball, walls, makePaddle rose 120 $ player1 game, makePaddle orange (-120) $ player2 game]
  where
    ball = uncurry translate (ballLoc game) $ color ballColor $ circleSolid 10
    ballColor = dark red

    wall :: Float -> Picture
    wall offset = 
      translate 0 offset $ color wallColor $ rectangleSolid 270 10

    wallColor = greyN 0.5
    walls = pictures [wall 150, wall (-150)]

    makePaddle :: Color -> Float -> Float -> Picture
    makePaddle col x y = pictures
      [ translate x y $ color col $ rectangleSolid 26 86
      , translate x y $ color paddleColor $ rectangleSolid 20 80
      ]
    paddleColor = light (light blue)

moveBall :: Float -> PongGame -> PongGame
moveBall seconds game = game { ballLoc = (x', y') }
  where
    (x, y) = ballLoc game
    (vx, vy) = ballVeloc game

    x' = x + vx * seconds
    y' = y + vy * seconds

type Radius = Float
type Position = (Float, Float)

paddleBounce :: PongGame -> PongGame
paddleBounce game = game { ballVeloc = (vx', vy) }
  where
    radius = 10
    (vx, vy) = ballVeloc game
    
    vx' = if paddleCollision game
          then -vx
          else vx


paddleCollision :: PongGame -> Bool
paddleCollision game = leftPaddleCollision || rightPaddleCollision
  where
    (x, y) = ballLoc game
    radius = 10
    paddleWidth = 26
    paddleHeight = 86
    leftPaddleX = -120
    rightPaddleX = 120

    leftPaddleCollision = 
      x - radius <= leftPaddleX + paddleWidth / 2 &&
      x + radius >= leftPaddleX - paddleWidth / 2 &&
      y + radius >= player1 game - paddleHeight / 2 &&
      y - radius <= player1 game + paddleHeight / 2

    rightPaddleCollision = 
      x + radius >= rightPaddleX - paddleWidth / 2 &&
      x - radius <= rightPaddleX + paddleWidth / 2 &&
      y + radius >= player2 game - paddleHeight / 2 &&
      y - radius <= player2 game + paddleHeight / 2

wallBounce :: PongGame -> PongGame
wallBounce game = game { ballVeloc = (vx, vy') }
  where 
    radius = 10
    (vx, vy) = ballVeloc game

    vy' = if wallCollision (ballLoc game) radius
          then
            -vy
          else
            vy


wallCollision :: Position -> Radius -> Bool
wallCollision (_, y) radius = topCollision || bottomCollision
  where
    topCollision = y - radius <= -fromIntegral width / 2
    bottomCollision = y + radius >= fromIntegral width / 2

window :: Display
window = InWindow "Pong" (width, height) (0, 0)

background :: Color
background = black

fps :: Int
fps = 60

update :: ViewPort -> Float -> PongGame -> PongGame
-- function composition operator
update _ seconds = wallBounce . paddleBounce . moveBall seconds

main :: IO ()
main = animate window background frame
  where
    frame :: Float -> Picture
    frame seconds = render $ moveBall seconds initialState









