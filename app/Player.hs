module Player where

import Board

data Player = 
  Player {
    location :: Point,
    currDirection :: Direction, 
    nextDirection :: Direction, 
    velocity :: Point
  }
  deriving (Eq, Show)