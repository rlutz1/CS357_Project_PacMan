module Player where

import Brillo
import Board

data Player = 
  Player {
    location :: Point,
    currDirection :: Direction, 
    nextDirection :: Direction, 
    velocity :: Point
  }
  deriving (Eq, Show)


genPlayer :: Player 
genPlayer = Player playerStartPoint NONE NONE (0, 0)