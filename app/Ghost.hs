module Ghost where

import Brillo
import Board

data Ghost = 
  Ghost {
    location :: Point,
    path :: (Destination, [Track]),
    currDirection :: Direction, 
    nextDirection :: Direction
 }
 deriving (Eq, Show)

updateGhosts :: [Ghost] -> [Ghost]
updateGhosts gs = gs