module Main where

import Graphics.Gloss
import Graphics.Gloss.Data.ViewPort
import Graphics.Gloss.Interface.Pure.Game

width, height :: Int
paddleWidth, paddleInnerWidth, paddleHeight :: Float
ballInitialVelocity :: (Float, Float)
player1InitPosition, player2InitPosition :: Float
paddleMargin :: Float
bottomTopLineThickness :: Float
ballRadius :: Float

width = 600
height = 600
paddleWidth = 26
paddleHeight = 86
paddleInnerWidth = 20
ballInitialVelocity = (150, 10)
player1InitPosition = 0
player2InitPosition = 0
paddleMargin = 25
bottomTopLineThickness = 10
ballRadius = 10

data Bounce = BounceUp | BounceDown deriving Show

data PongGame = Game
  { ballLoc :: (Float, Float) -- x y location for pong ball
  , ballVeloc :: (Float, Float) -- pong ball velocity
  , player1 :: Float -- left player paddle height where 0 is middle screen
  , player2 :: Float --right player paddle height
  , paused :: Bool
  , wKeyPressed :: Bool
  , sKeyPressed :: Bool
  , arrUpKeyPressed :: Bool
  , arrDownKeyPressed :: Bool
  , seed :: Int
  , nextBounceDirection :: Bounce
  } deriving Show

initialState :: PongGame
initialState = Game
  { ballLoc = (0, 0)
  , ballVeloc = ballInitialVelocity
  , player1 = player1InitPosition
  , player2 = player2InitPosition 
  , paused = False
  , wKeyPressed = False
  , sKeyPressed = False
  , arrUpKeyPressed = False
  , arrDownKeyPressed = False
  , seed = 0
  , nextBounceDirection = BounceUp
  }


pseudoRandom :: Int -> Bounce -> Int -> Int
pseudoRandom seed BounceUp limit   = ((1103515245 * seed + 12345) `mod` limit)
pseudoRandom seed BounceDown limit = -(((1103515245 * seed + 12345) `mod` limit))

render :: PongGame -> Picture
render game =
  pictures [ball, walls, makePaddle rose (fromIntegral (height `div` 2) - paddleMargin) $ player1 game, makePaddle orange (-(fromIntegral (height `div` 2) - paddleMargin )) $ player2 game]
  where
    ball = uncurry translate (ballLoc game) $ color ballColor $ circleSolid ballRadius
    ballColor = dark red

    wall :: Float -> Picture
    wall offset = 
      translate 0 offset $ color wallColor $ rectangleSolid (fromIntegral (width - 50)) bottomTopLineThickness

    wallColor = greyN 0.5
    walls = pictures [wall (fromIntegral (height `div` 2)), wall (-(fromIntegral (height `div` 2)))]

    makePaddle :: Color -> Float -> Float -> Picture
    makePaddle col x y = pictures
      [ translate x y $ color col $ rectangleSolid paddleWidth paddleHeight
      , translate x y $ color paddleColor $ rectangleSolid (paddleInnerWidth) paddleHeight
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

paddleBounce game = game { ballVeloc = (vx', vy'), seed = newSeed, nextBounceDirection = nextBounce }
  where
    (vx, vy) = ballVeloc game
    
    bounceDirection = nextBounceDirection game

    oldSeed = seed game
    newSeed = oldSeed + 42

    hadCollision = paddleCollision game

    randomFac = pseudoRandom oldSeed bounceDirection 30

    nextBounce = if odd randomFac
                then BounceUp
                else BounceDown
 
    vx' = if paddleCollision game
          then -(vx + fromIntegral randomFac)
          else vx

    vy' = if paddleCollision game
          then vy + fromIntegral randomFac
          else vy


paddleCollision :: PongGame -> Bool
paddleCollision game = leftPaddleCollision || rightPaddleCollision
  where
    (x, y) = ballLoc game
    leftPaddleX = -fromIntegral (width `div` 2) + paddleWidth
    rightPaddleX = fromIntegral (width `div` 2) - paddleWidth

    leftPaddleCollision = 
      x - ballRadius <= leftPaddleX &&
      x + ballRadius >= leftPaddleX &&
      y + ballRadius >= player1 game - paddleHeight &&
      y - ballRadius <= player1 game + paddleHeight

    rightPaddleCollision = 
      x + ballRadius >= rightPaddleX &&
      x - ballRadius <= rightPaddleX &&
      y + ballRadius >= player2 game - paddleHeight &&
      y - ballRadius <= player2 game + paddleHeight
    
wallBounce :: PongGame -> PongGame
wallBounce game = game { ballVeloc = (vx, vy') }
  where 
    (vx, vy) = ballVeloc game

    vy' = if wallCollision (ballLoc game) ballRadius
          then
            -vy
          else
            vy

wallCollision :: Position -> Radius -> Bool
wallCollision (_, y) radius = topCollision || bottomCollision
  where
    topCollision = y - radius <= -fromIntegral width / 2
    bottomCollision = y + radius >= fromIntegral width / 2

handleKeys :: Event -> PongGame -> PongGame
handleKeys (EventKey (Char 'r') _ _ _) game =
  game { ballLoc = (0, 0) }
handleKeys (EventKey (Char 'p') Down _ _) game =
  game { paused = not (paused game) }
handleKeys (EventKey (Char 'w') keyState _ _) game =
  case keyState of
    Down -> game { wKeyPressed = True } 
    Up -> game { wKeyPressed = False } 
handleKeys (EventKey (Char 's') keyState _ _) game =
  case keyState of
    Down -> game { sKeyPressed = True } 
    Up -> game { sKeyPressed = False } 
handleKeys (EventKey (SpecialKey KeyUp) keyState _ _) game =
  case keyState of
    Down -> game { arrUpKeyPressed = True } 
    Up -> game { arrUpKeyPressed = False } 
handleKeys (EventKey (SpecialKey KeyDown) keyState _ _) game =
  case keyState of
    Down -> game { arrDownKeyPressed = True } 
    Up -> game { arrDownKeyPressed = False } 

handleKeys _ game = game

window :: Display
window = InWindow "Pong" (width, height) (0, 0)

background :: Color
background = black

fps :: Int
fps = 120

updatePaddles :: PongGame -> PongGame
updatePaddles game = game { player1 = newPlayer1Pos, player2 = newPlayer2Pos }
  where
    newPlayer1Pos
      | wKeyPressed game && player1 game + 2 <= fromIntegral (height `div` 2) - (paddleHeight / 2) - bottomTopLineThickness = player1 game + 2
      | sKeyPressed game && player1 game - 2 >= -fromIntegral (height `div` 2) + (paddleHeight / 2) + bottomTopLineThickness = player1 game - 2
      | otherwise = player1 game

    newPlayer2Pos
      | arrUpKeyPressed game && player2 game + 2 <= fromIntegral (height `div` 2) - (paddleHeight / 2) - bottomTopLineThickness = player2 game + 2
      | arrDownKeyPressed game && player2 game - 2 >= -fromIntegral (height `div` 2) + (paddleHeight / 2) + bottomTopLineThickness = player2 game - 2
      | otherwise = player2 game


-- if ballLoc (N, m) passed width or height, reset ballLoc and set game to paused
gameFinished :: PongGame -> PongGame 
gameFinished game = if abs (fst (ballLoc game)) >= fromIntegral width / 2
                    then game { ballLoc = (0, 0), player1 = player1InitPosition, player2 = player2InitPosition, paused = True }
                    else game


update :: Float -> PongGame -> PongGame
-- function composition operator
-- on update, check if game is paused. If it is, return same pong game
update seconds game 
  | paused game = game
  | otherwise = updatePaddles . gameFinished  . paddleBounce . wallBounce . moveBall seconds $ game

main :: IO ()
main = play window background fps initialState render handleKeys update




