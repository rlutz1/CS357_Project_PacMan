module Data where

import Brillo

data Board = 
  Board {
    tiles :: [Tile], -- for display
    pivots :: [Pivot], -- for actual movement
    collectibles :: [Collectible], -- collectibles, maybe within pivot later?
    lives :: Int, -- for displaying lives
    score :: Int, -- for displaying the current score
    drawB :: Board -> [Picture],
    updateB :: Board -> Board, 
    player :: Player,
    ghosts :: [Ghost]
  }
  -- deriving (Eq, Show) 

data Tile = Tile Color Boundary -- purely for drawing the tile map, aesthetics alone
  deriving (Eq, Show) 

-- up down left right
data Pivot = Pivot Point (Neighbor, Neighbor, Neighbor, Neighbor)
  deriving (Eq, Show)  

data Neighbor = Null | Neighbor Destination [Track]
  deriving (Eq, Show) 

data Destination = Destination Point
  deriving (Eq, Show) 
-- data Track = Track Point 
--   deriving (Eq, Show) 

data Boundary = 
  Boundary {
    b :: Bottom,
    t :: Top,
    l :: Left,
    r :: Right
  }
  deriving (Eq, Show) 

type Bottom = Float -- for ease of reading lol
type Top = Float
type Left = Float
type Right = Float

type Track = Point

data Direction = UP | DOWN | LEFT | RIGHT | NONE
    deriving (Enum, Eq, Show)

-- effect, score value color for drawing
data Collectible = Eaten Point | Collectible Effect Int Color Point
  deriving (Eq, Show) 

data Effect = NoEffect | GhostsOff
  deriving (Eq, Show) 

data Player = 
  Player {
    locationP :: Point,
    pathP :: (Destination, [Track]),
    currDirectionP :: Direction, 
    nextDirectionP :: Direction,
    drawP :: Player -> Picture,
    updateP :: Player -> Board -> Player,
    collDetection :: Bool
  }
  
data Ghost = 
  Ghost {
    locationG :: Point,
    pathG :: (Destination, [Track]),
    currDirectionG :: Direction, 
    nextDirectionG :: Direction,
    drawG :: Ghost -> Picture,
    updateG :: Ghost -> Board -> Ghost
 }