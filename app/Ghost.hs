module Ghost where

import Brillo
import Board

data Ghost = 
  Ghost {
    location :: Point,
    currDirection :: Direction, 
    nextDirection :: Direction, 
    velocity :: Point
 }
 deriving (Eq, Show)