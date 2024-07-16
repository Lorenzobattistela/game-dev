module Game where

import Foreign.C.Types (CInt)
import Data.Array
import Data.Ord (comparing)
import Data.List (maximumBy)

windowWidth :: CInt
windowWidth = 950

windowHeight :: CInt
windowHeight = 800

spriteWidth :: Float
spriteWidth = 26

spriteHeight :: Float
spriteHeight = 36

frameRows :: Float
frameRows = 5

frameCols :: Float
frameCols = 3

animationFrames :: Float
animationFrames = frameRows * frameCols

enemyWidth :: Float
enemyWidth = 36

enemyHeight :: Float
enemyHeight = 36

enemyCols :: Float
enemyCols = 3

enemyRows :: Float 
enemyRows = 4

enemyAnimationFrames :: Float
enemyAnimationFrames = enemyCols * enemyRows

numEnemies :: Float
numEnemies = 7

speed :: Float
speed = 2

fps :: Int
fps = 60

frameDelay :: Int
frameDelay = 1000 `div` fps

data LoopState = Quit | Running | Won | Lost deriving (Show, Enum, Bounded, Eq)

data Controls = W | A | S | D deriving (Show, Enum, Bounded, Eq, Ord)

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
  deriving (Show, Enum, Bounded, Eq)

data FrameDirection = FRONT | LEFT | RIGHT | BACK deriving (Show, Enum, Bounded, Eq, Ord)

data Frames = Frames
  { frameDirection :: FrameDirection
  , frames ::[FramesType]
  , nextFrame :: FramesType
  } deriving (Show)

data Position = Position
  { x :: Float
  , y :: Float
  } deriving (Show, Eq)

data Enemy = Enemy
  { ePosition :: Position
  , eLife :: Int
  , damage :: Int
  , eSpeed :: Float
  , eDead :: Bool
  , currentFrame :: FramesType 
  } deriving (Show, Eq)

data Player = Player
  { pPosition :: Position
  , life :: Int
  , pSpeed :: Float
  , pDead :: Bool
  , pCurrentFrame :: FramesType
  } deriving (Show, Eq)

controls :: Array Int Bool
controls = array (0, 3) [(i, False) | i <- [0..3]]

enemyHit :: Player -> Enemy -> Bool
enemyHit player enemy =
          let  px = x (pPosition player)
               py = y (pPosition player)
               ex = x (ePosition enemy )
               ey = y (ePosition enemy ) 
          in px < ex + enemyWidth  &&
             px + spriteWidth > ex &&
             py < ey + enemyHeight &&
             py + spriteHeight > ey


hitAnyEnemy :: Player -> [Enemy] -> Bool
hitAnyEnemy player enemies = any (enemyHit player) enemies

-- return the enemy object with the position updated. The calculu
updateEnemyPosition :: Player -> Enemy -> Enemy
updateEnemyPosition p e =
  let direction = Position (x (pPosition p) - x (ePosition e)) (y (pPosition p) - y (ePosition e))
      lengthDist = sqrt (x direction * x direction + y direction * y direction)
      normalizedDirection = Position ((x direction) / lengthDist) ((y direction) / lengthDist)
      newPosition = Position
        ((x (ePosition e)) + (x normalizedDirection) * (eSpeed e))
        ((y (ePosition e)) + (y normalizedDirection) * (eSpeed e))
      newFrame = determineFrame normalizedDirection
   in e { ePosition = newPosition , currentFrame = newFrame }


-- return the next correct frame to the animation
determineFrame :: Position -> FramesType
determineFrame dir
  | abs (x dir) > abs (y dir) = if (x dir) > 0 then BACK_WALK_LEFT_HAND else RIGHT_WALK_RIGHT_HAND
  | (y dir) > 0 = LEFT_WALK_RIGHT_HAND
  | (x dir) > 0 = BACK_WALK_LEFT_HAND
  | otherwise = RIGHT_WALK_RIGHT_HAND


updateEnemiesPosition :: Player -> [Enemy] -> [Enemy]
updateEnemiesPosition p = map (updateEnemyPosition p) . filter (not . eDead)

pseudoRandom :: Int -> Int
pseudoRandom seed = (1103515245 * seed + 12345) `mod` 2147483648

createEnemyAtRandomPos :: Int -> Int -> Int -> Float -> Enemy
createEnemyAtRandomPos seed life damage speed =
  let
    windowWidth = 950
    windowHeight = 800
    randX = pseudoRandom seed
    randY = pseudoRandom (seed + 1)

    enemyWidth = 36
    enemyHeight = 36

    posX = fromIntegral (randX `mod` (windowWidth - enemyWidth))
    posY = fromIntegral (randY `mod` (windowHeight - enemyHeight))
  in
    Enemy
      { ePosition = Position posX posY
      , eLife = life
      , damage = damage
      , eSpeed = speed
      , eDead = False
      , currentFrame = FRONT_STANDING
      }

-- this functions create N enemies in different positions, with the same life, damage and speed
createEnemies :: Int -> Int -> Int -> Float -> [Enemy]
createEnemies numEnemies life damage speed = 
  let seeds = [1..numEnemies]
  in map (\seed -> createEnemyAtRandomPos seed life damage speed) seeds

updateFrontFrame :: FramesType -> FramesType
updateFrontFrame FRONT_WALK_LEFT_HAND = FRONT_WALK_RIGHT_HAND
updateFrontFrame FRONT_WALK_RIGHT_HAND = FRONT_STANDING
updateFrontFrame FRONT_STANDING = FRONT_WALK_LEFT_HAND
updateFrontFrame other = other

updateLeftFrame :: FramesType -> FramesType
updateLeftFrame LEFT_WALK_LEFT_HAND = LEFT_WALK_RIGHT_HAND
updateLeftFrame LEFT_WALK_RIGHT_HAND = LEFT_WALK_STANDING
updateLeftFrame LEFT_WALK_STANDING = LEFT_WALK_LEFT_HAND
updateLeftFrame other = other

updateRightFrame :: FramesType -> FramesType
updateRightFrame RIGHT_WALK_LEFT_HAND = RIGHT_WALK_RIGHT_HAND
updateRightFrame RIGHT_WALK_RIGHT_HAND = RIGHT_WALK_STANDING
updateRightFrame RIGHT_WALK_STANDING = RIGHT_WALK_LEFT_HAND
updateRightFrame other = other

updateBackFrame :: FramesType -> FramesType
updateBackFrame BACK_WALK_LEFT_HAND = BACK_WALK_RIGHT_HAND
updateBackFrame BACK_WALK_RIGHT_HAND = BACK_WALK_STANDING
updateBackFrame BACK_WALK_STANDING = BACK_WALK_LEFT_HAND
updateBackFrame other = other
































