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


{-
first, if the player currdir /= nextdir, check 
  if they're at close enough to end of their track
    grab the pivot (via player destination) from the board, try to grab the corresponding neighbor from that pivot
    if its null
      instead keep trying to go the same direction as before, can't turn there
      if that is null, stop moving -- keep same location
want to clean this up, works well enough to continue testing, but there's def some gunk to cut out
-}
movePlayer :: Player -> Board -> Player
movePlayer (Player loc (dest, []) curr next d u coll) _ = Player loc (dest, [loc]) curr next d u coll
movePlayer (Player _ (dest, t:ts) curr next d u coll) b
  -- | opposing curr next = changeDir (Player t (dest, ts) curr next d u coll) b
  | curr /= next && closeEnough ts = changeDir (Player t (dest, ts) curr next d u coll) b
  | otherwise = sameDir (Player t (dest, ts) curr curr d u coll) b
    
-- queueTracks :: Player -> Board -> Player
-- queueTracks (Player loc (dest, ts) curr next) b -- we are attempting oto queue up the next move
--   | curr /= next = changeDir (Player loc (dest, ts) curr next) b 
--   | otherwise = sameDir (Player loc (dest, ts) curr next) b 
 
 --genTracks (1.0) dir from dest
-- todo clean up

getTracks :: Maybe Pivot -> Direction -> Neighbor
getTracks Nothing _ = Null
getTracks (Just (Pivot _ (upN, downN, leftN, rightN))) dir
  | dir == UP = upN
  | dir == DOWN = downN
  | dir == LEFT = leftN
  | dir == RIGHT = rightN
  | otherwise = Null


changeDir :: Player -> Board -> Player
changeDir (Player loc ( point, ts) curr next d u coll) b 
  | nextNeighbor == Null = sameDir (Player loc (point, ts) curr curr d u coll) b 
  -- | opposing curr next Player loc (deconDest nextNeighbor, (genMovement nextNeighbor)) next next d u coll
  | otherwise = Player loc (deconDest nextNeighbor, ts ++ (genMovement nextNeighbor)) next next d u coll
  where 
    nextPiv = getPivot point b
    nextNeighbor = getTracks nextPiv next 


opposing :: Direction -> Direction -> Bool
opposing LEFT RIGHT = True
opposing UP DOWN = True
opposing _ _ = False

genMovement :: Neighbor -> [Track]
genMovement Null = error "dont' give this null!!"
genMovement (Neighbor dir dest from) = genTracks (1.0) dir from dest

-- todo clean up
sameDir :: Player -> Board -> Player
sameDir (Player loc ( point, []) curr _ d u coll) _ = Player loc ( point, []) curr curr d u coll
sameDir (Player loc ( point, [t]) curr _ d u coll) b
  | nextNeighbor == Null = Player loc ( point, [t]) curr curr d u coll
  | otherwise = Player loc (deconDest nextNeighbor, t : (genMovement nextNeighbor)) curr curr d u coll
  where 
    nextPiv = getPivot point b
    nextNeighbor = getTracks nextPiv curr
sameDir (Player loc ( point, ts) curr next d u coll) _ = Player loc ( point, ts) curr next d u coll 

closeEnough :: [a] -> Bool
closeEnough xs = length xs <= 25

{-
------------------------------------------------------------
PLAYER DRAWING FUNCTIONS
------------------------------------------------------------
-}

drawPlayer :: Player -> Picture
drawPlayer (Player (x, y) _ _ _ _ _ _) = color yellow (translate x y (thickCircle 10 20))

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


