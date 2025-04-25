module Ghost where

import Brillo
import Board

data Ghost = 
  Ghost {
    location :: Point,
    path :: (Destination, [Track]),
    currDirection :: Direction, 
    nextDirection :: Direction,
    drawG :: Ghost -> Picture,
    updateG :: Ghost -> Board -> Ghost
 }


updateGhosts :: [Ghost] -> [Ghost]
updateGhosts gs = gs

drawGhosts :: [Ghost] -> [Picture]
drawGhosts gs = [Circle 15] -- todo