module Data where

import Brillo

{-
  @author Roxanne Lutz
  all data and type declarations.
  made life much easier to have all of these
  in one place for ease of changing.
-}

-- the game board, or main world really
data Board = 
  Board {
    tiles :: [Tile], -- for display
    pivots :: [Pivot], -- for actual movement
    collectibles :: [Collectible], -- collectibles
    lives :: Int, -- for displaying lives
    score :: Int, -- for displaying the current score
    drawB :: Board -> [Picture], -- draw board func
    updateB :: Board -> Float ->  Board, -- update board func
    player :: Player, -- player on board
    ghosts :: [Ghost], -- ghosts on board
    gameOver :: Bool, -- if the game is over or not
    effectTimers :: [(Effect, Float, Float)] -- any effect timers needed
  }

-- purely for drawing the tile map, aesthetics alone
data Tile = Tile Color Boundary
  deriving (Eq, Show) 

-- a center point with a point and all neighbors, null or otherwise.
-- UP, DOWN, LEFT, RIGHT order by default of neighbors.
data Pivot = Pivot Point (Neighbor, Neighbor, Neighbor, Neighbor)
  deriving (Eq, Show)  

-- a neighbor, with what their relative direction is from a pivot,
-- their point 
data Neighbor = Null | Neighbor Direction To From
  deriving (Eq, Show) 

-- a boundary for a tile. helpful for graphical component
data Boundary = 
  Boundary {
    bottom :: Bottom,
    top :: Top,
    left :: Left,
    right :: Right
  }
  deriving (Eq, Show) 

-- direction Enum, sort of :)
data Direction = UP | DOWN | LEFT | RIGHT | NONE
    deriving (Enum, Eq, Show)

-- a collectible with effect, score, value, color for drawing
data Collectible = Collectible Effect Int Color Point
  deriving (Eq, Show) 

-- potential collectible effect. ghosts off contains the length of time for effect
data Effect = NoEffect | GhostsOff Float
  deriving (Eq, Show) 

-- the player with all its needed data
data Player = 
  Player {
    locationP :: Point, -- current location
    pathP :: (To, [Track]), -- the path they're headed on
    currDirectionP :: Direction, -- current dir
    nextDirectionP :: Direction, -- next direction
    drawP :: Player -> Picture, -- drawing func
    updateP :: Player -> Board -> Player, -- updating func
    collDetection :: Bool -- if collision detection is on
  }

-- the ghost with all its needed data
data Ghost = 
  Ghost {
    name :: String, -- name of ghosty
    locationG :: Point, -- current location
    pathG :: (To, [Track]), -- current path
    drawG :: Ghost -> Picture, -- draw function
    updateG :: Ghost -> Board -> Ghost, --update function
    inf :: [Int] -- infinite list for path selection shuffling
 }

-- type synonyms for ease of reading 
type Bottom = Float
type Top = Float
type Left = Float
type Right = Float
type Track = Point
type To = Point
type From = Point
