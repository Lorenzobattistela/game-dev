module Main where

import qualified Data.Map as Map

import Graphics.Gloss
import Graphics.Gloss.Data.ViewPort
import Graphics.Gloss.Interface.Pure.Game

type Radius = Float
type Position = (Float, Float)
type Velocity = (Float, Float) -- (x, y) where x is the horizontal velocity and y is the vertical

data Bounce = BounceUp | BounceDown deriving Show

data ControlKey = WKey | SKey | ArrUpKey | ArrDownKey deriving (Eq, Ord, Show)
type Controls = Map.Map ControlKey Bool

data PongGame = Game
  { ballLoc :: Position -- x y location for pong ball
  , ballVeloc :: Velocity -- pong ball velocity
  , player1 :: Float -- left player paddle height where 0 is middle screen
  , player2 :: Float -- right player paddle height
  , paused :: Bool
  , controls :: Controls
  , seed :: Int
  , nextBounceDirection :: Bounce
  } deriving Show


width, height :: Int
paddleWidth, paddleInnerWidth, paddleHeight :: Float
ballInitialVelocity :: Velocity 
player1InitPosition, player2InitPosition :: Float
paddleMargin :: Float
bottomTopLineThickness :: Float
ballRadius :: Float
window :: Display
background :: Color
fps :: Int

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
window = InWindow "Pong" (width, height) (0, 0)
background = black
fps = 120


initialControls :: Controls
initialControls = Map.fromList
  [ (WKey, False)
  , (SKey, False)
  , (ArrUpKey, False)
  , (ArrDownKey, False)
  ]

initialState :: PongGame
initialState = Game
  { ballLoc = (0, 0)
  , ballVeloc = ballInitialVelocity
  , player1 = player1InitPosition
  , player2 = player2InitPosition 
  , paused = False
  , controls = initialControls
  , seed = 0
  , nextBounceDirection = BounceUp
  }

updateControl :: PongGame -> ControlKey -> Bool -> PongGame
updateControl game key value = game { controls = Map.insert key value (controls game) }

isKeyPressed :: PongGame -> ControlKey -> Bool
isKeyPressed game key = Map.findWithDefault False key (controls game)


pseudoRandom :: Int -> Bounce -> Int -> Int
pseudoRandom seed BounceUp limit   = ((1103515245 * seed + 12345) `mod` limit)
pseudoRandom seed BounceDown limit = -(((1103515245 * seed + 12345) `mod` limit))

render :: PongGame -> Picture
render game =
  let
    ballColor = dark red
    ball = uncurry translate (ballLoc game) $ color ballColor $ circleSolid ballRadius

    wallColor = greyN 0.5
    wall :: Float -> Picture
    wall offset = 
      translate 0 offset $ color wallColor $ rectangleSolid (fromIntegral (width - 50)) bottomTopLineThickness
    walls = pictures [wall (fromIntegral (height `div` 2)), wall (-(fromIntegral (height `div` 2)))]

    paddleColor = light (light blue)
    makePaddle :: Color -> Float -> Float -> Picture
    makePaddle col x y = pictures
      [ translate x y $ color col $ rectangleSolid paddleWidth paddleHeight
      , translate x y $ color paddleColor $ rectangleSolid paddleInnerWidth paddleHeight
      ]
  in
    pictures [ball, walls, 
              makePaddle rose (fromIntegral (height `div` 2) - paddleMargin) $ player1 game, 
              makePaddle orange (-(fromIntegral (height `div` 2) - paddleMargin)) $ player2 game]

moveBall :: Float -> PongGame -> PongGame
moveBall seconds game =
    let (x, y) = ballLoc game
        (vx, vy) = ballVeloc game
        x' = x + vx * seconds
        y' = y + vy * seconds
    in game { ballLoc = (x', y') }


paddleBounce game = 
    let (vx, vy) = ballVeloc game
        bounceDirection = nextBounceDirection game
        oldSeed = seed game
        newSeed = oldSeed + 42
        hadCollision = paddleCollision game
        randomFac = pseudoRandom oldSeed bounceDirection 30

        nextBounce = 
          if odd randomFac
          then BounceUp
          else BounceDown

        vx' =
          if paddleCollision game
          then -(vx + fromIntegral randomFac)
          else vx

        vy' = 
          if paddleCollision game
          then vy + fromIntegral randomFac
          else vy
    in game { ballVeloc = (vx', vy'), seed = newSeed, nextBounceDirection = nextBounce }

paddleCollision :: PongGame -> Bool
paddleCollision game =
  let
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
  in
    leftPaddleCollision || rightPaddleCollision

wallBounce :: PongGame -> PongGame
wallBounce game =
  let
    (vx, vy) = ballVeloc game
    vy' = 
      if wallCollision (ballLoc game) ballRadius
      then -vy
      else vy
  in
    game { ballVeloc = (vx, vy') }

wallCollision :: Position -> Radius -> Bool
wallCollision (_, y) radius =
    let topCollision = y - radius <= -fromIntegral width / 2
        bottomCollision = y + radius >= fromIntegral width / 2
    in topCollision || bottomCollision

handleKeys :: Event -> PongGame -> PongGame
handleKeys (EventKey (Char 'r') _ _ _) game =
  game { ballLoc = (0, 0) }
handleKeys (EventKey (Char 'p') Down _ _) game =
  game { paused = not (paused game) }
handleKeys (EventKey (Char 'w') keyState _ _) game =
  updateControl game WKey (keyState == Down)
handleKeys (EventKey (Char 's') keyState _ _) game =
  updateControl game SKey (keyState == Down)
handleKeys (EventKey (SpecialKey KeyUp) keyState _ _) game =
  updateControl game ArrUpKey (keyState == Down)
handleKeys (EventKey (SpecialKey KeyDown) keyState _ _) game =
  updateControl game ArrDownKey (keyState == Down)
handleKeys _ game = game

updatePaddles :: PongGame -> PongGame
updatePaddles game =
  let
    paddleTopLimit = fromIntegral (height `div` 2) - (paddleHeight / 2) - bottomTopLineThickness
    paddleBottomLimit = -fromIntegral (height `div` 2) + (paddleHeight / 2) + bottomTopLineThickness
    
    moveUp currentPos = currentPos + 2
    moveDown currentPos = currentPos - 2
    
    newPlayer1Pos =
      if isKeyPressed game WKey && moveUp (player1 game) <= paddleTopLimit
        then moveUp (player1 game)
      else if isKeyPressed game SKey && moveDown (player1 game) >= paddleBottomLimit
        then moveDown (player1 game)
      else player1 game
    
    newPlayer2Pos =
      if isKeyPressed game ArrUpKey && moveUp (player2 game) <= paddleTopLimit
        then moveUp (player2 game)
      else if isKeyPressed game ArrDownKey && moveDown (player2 game) >= paddleBottomLimit
        then moveDown (player2 game)
      else player2 game
  in
    game { player1 = newPlayer1Pos, player2 = newPlayer2Pos }

-- if ballLoc (N, m) passed width or height, reset ballLoc and set game to paused
gameFinished :: PongGame -> PongGame 
gameFinished game = 
  if abs (fst (ballLoc game)) >= fromIntegral width / 2
  then game { ballLoc = (0, 0), player1 = player1InitPosition, player2 = player2InitPosition, paused = True }
  else game

-- function composition operator
-- on update, check if game is paused. If it is, return same pong game
update :: Float -> PongGame -> PongGame
update seconds game 
  | paused game = game
  | otherwise = updatePaddles . gameFinished  . paddleBounce . wallBounce . moveBall seconds $ game

main :: IO ()
main = play window background fps initialState render handleKeys update




