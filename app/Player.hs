{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
module Player where

import Brillo
-- -- import Board
import Data
import Utils





{-
------------------------------------------------------------
UPDATE FUNCTIONS
------------------------------------------------------------
-}

updatePlayer :: Player -> Board -> Player
updatePlayer = movePlayer

{-
------------------------------------------------------------
PLAYER MOVEMENT FUNCTIONS
------------------------------------------------------------
-}

-- actual function for the moving the player based on input.
-- player can only queue a move if they are close enough to a pivot,
-- acknowledged limitation that should be eased up in future.
movePlayer :: Player -> Board -> Player
movePlayer (Player loc (dest, []) curr next d u coll) _ = Player loc (dest, [loc]) curr next d u coll
movePlayer (Player _ (dest, t:ts) curr next d u coll) b
  | curr /= next && nearPivot ts = changeDir (Player t (dest, ts) curr next d u coll) b
  | otherwise = sameDir (Player t (dest, ts) curr curr d u coll) b
    
-- attempt to change direction if that is a valid pathway. otherwise keep moving in same direction
changeDir :: Player -> Board -> Player
changeDir (Player loc (point, ts) curr next d u coll) b 
  | nextNeighbor == Null = sameDir (Player loc (point, ts) curr curr d u coll) b 
  | otherwise = Player loc (deconDest nextNeighbor, ts ++ (genMovement nextNeighbor)) next next d u coll
  where 
    nextPiv = getPivot point b
    nextNeighbor = getSpecificNeighbor nextPiv next 

-- generate a list of movements based on which neighbor we want to head to
genMovement :: Neighbor -> [Track]
genMovement Null = error "dont' give this null!!"
genMovement (Neighbor dir dest from) = genTracks defaultSpeed dir from dest

-- attempt to go in the same direction if you run out of path and don't change direction. don't move at all when hitting walls.
sameDir :: Player -> Board -> Player
sameDir (Player loc (point, []) curr _ d u coll) _ = Player loc ( point, []) curr curr d u coll
sameDir (Player loc (point, [t]) curr _ d u coll) b
  | nextNeighbor == Null = Player loc (point, [t]) curr curr d u coll
  | otherwise = Player loc (deconDest nextNeighbor, t : (genMovement nextNeighbor)) curr curr d u coll
  where 
    nextPiv = getPivot point b
    nextNeighbor = getSpecificNeighbor nextPiv curr
sameDir (Player loc (point, ts) curr next d u coll) _ = Player loc ( point, ts) curr next d u coll 

-- simple helper to see if pacman is close enough to request a change in direction
nearPivot :: [a] -> Bool
nearPivot xs = length xs <= 25

{-
------------------------------------------------------------
PLAYER DRAWING FUNCTIONS
------------------------------------------------------------
-}

-- draw the player
drawPlayer :: Player -> Picture
drawPlayer (Player (x, y) _ _ _ _ _ _) = color yellow (translate x y (thickCircle playerRadius playerThickness))

{-
------------------------------------------------------------
PLAYER CONSTANTS
------------------------------------------------------------
-}

playerStartPoint :: Point
playerStartPoint = (-475, -475)

playerInitLives :: Int
playerInitLives = 3 -- todo: 3 for final

playerInitScore :: Int
playerInitScore = 0

playerRadius :: Float
playerRadius = 10

playerThickness :: Float 
playerThickness = 20
