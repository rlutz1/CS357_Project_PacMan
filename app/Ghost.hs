module Ghost where

import Board

data Ghost = 
  Ghost {
    location :: Point,
    currDirection :: Direction, 
    nextDirection :: Direction, 
    velocity :: Point
 }
 deriving (Eq, Show)