module Ghost where

import Brillo
import Board

data Ghost = 
  Ghost {
    location :: Point,
    path :: (Destination, [Track]),
    currDirection :: Direction, 
    nextDirection :: Direction, 
    velocity :: Point
 }
 deriving (Eq, Show)