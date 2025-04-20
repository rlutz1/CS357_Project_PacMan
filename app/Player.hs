module Player where

import Brillo
import Board

data Player = 
  Player {
    location :: Point,
    path :: (Destination, [Track]),
    currDirection :: Direction, 
    nextDirection :: Direction, 
    velocity :: Point
  }
  deriving (Eq, Show)


genPlayer :: Player 
genPlayer = Player playerStartPoint (None, []) NONE NONE (0, 0)