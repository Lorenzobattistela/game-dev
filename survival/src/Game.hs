module Game where

import Foreign.C.Types (CInt)

windowWidth :: CInt
windowWidth = 950

windowHeight :: CInt
windowHeight = 800

spriteWidth :: Int
spriteWidth = 26

spriteHeight :: Int
spriteHeight = 36

frameRows :: Int
frameRows = 5

frameCols :: Int
frameCols = 3

animationFrames :: Int
animationFrames = frameRows * frameCols

enemyWidth :: Int
enemyWidth = 36

enemyHeight :: Int
enemyHeight = 36

enemyCols :: Int
enemyCols = 3

enemyRows :: Int 
enemyRows = 4

enemyAnimationFrames :: Int
enemyAnimationFrames = enemyCols * enemyRows

numEnemies :: Int
numEnemies = 7

speed :: Int
speed = 2

fps :: Int
fps = 60

frameDelay :: Int
frameDelay = 1000 `div` fps

data GameState = Quit | Running | Won | Lost deriving (Show, Enum, Bounded)

data Controls = W | A | S | D deriving (Show, Enum, Bounded)

data FramesType = 
    FRONT_WALK_RIGHT_HAND
  | FRONT_STANDING
  | FRONT_WALK_LEFT_HAND
  | LEFT_WALK_RIGHT_HAND
  | LEFT_WALK_STANDING
  | LEFT_WALK_LEFT_HAND
  | RIGHT_WALK_RIGHT_HAND
  | RIGHT_WALK_STANDING
  | RIGHT_WALK_LEFT_HAND
  | BACK_WALK_RIGHT_HAND
  | BACK_WALK_STANDING
  | BACK_WALK_LEFT_HAND
  deriving (Show, Enum, Bounded)

data FrameDirection = FRONT | LEFT | RIGHT | BACK deriving (Show, Enum, Bounded)

data Frames = Frames
  { frameDirection :: FrameDirection
  , frames ::[FramesType]
  , nextFrame :: FramesType
  } deriving (Show)













