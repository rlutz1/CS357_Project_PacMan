module Player where

import Brillo
import Board

data Player = 
  Player {
    location :: Point,
    path :: (Destination, [Track]),
    currDirection :: Direction, 
    nextDirection :: Direction
  }
  deriving (Eq, Show)


genPlayer :: Player 
genPlayer = Player playerStartPoint (None, []) NONE NONE

movePlayer :: Player -> Player
movePlayer = undefined

updatePlayer :: Player -> Player
updatePlayer p = p