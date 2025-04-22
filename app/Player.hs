{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
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
genPlayer = Player playerStartPoint (Destination playerStartPoint, []) NONE NONE

{-
first, if the player currdir /= nextdir, check 
  (1) if they're at the end of their track
    grab the pivot (via player destination) from the board, try to grab the corresponding neighbor from that pivot
    if its null
      instead keep trying to go the same direction as before, can't turn there
      if that is null, stop moving -- keep same location
-}
movePlayer :: Player -> Board -> Player
movePlayer (Player loc (dest, []) curr next) b = (Player loc (dest, [loc]) curr next)
movePlayer (Player loc (dest, t:ts) curr next) b
  | closeEnough (ts) = queueTracks (Player t (dest, ts) curr next) b
  | otherwise = Player t (dest, (ts)) curr curr
    
queueTracks :: Player -> Board -> Player
queueTracks (Player loc (dest, ts) curr next) b -- exactly one move left queued
  | curr /= next = changeDir (Player loc (dest, ts) curr next) b 
  | otherwise = sameDir (Player loc (dest, ts) curr next) b 

-- movePlayer (Player loc (dest, [t]) curr next) b -- exactly one move left queued
--   | curr /= next = changeDir (Player loc (dest, [t]) curr next) b 
--   | otherwise = sameDir (Player loc (dest, [t]) curr next) b 
-- movePlayer (Player loc (dest, (t:ts)) curr next) _ = (Player t (dest, (ts)) curr curr)

changeDir :: Player -> Board -> Player
changeDir (Player loc ((Destination point), ts) curr next) b 
  | nextTracks == Null = sameDir (Player loc ((Destination point), ts) curr curr) b 
  | otherwise = (Player loc (deconDestination nextTracks, ts ++ deconTracks nextTracks) next next)
  where 
    nextPiv = getPivot point b
    nextTracks = getTracks nextPiv next




sameDir :: Player -> Board -> Player
sameDir (Player loc ((Destination point), []) curr next) b = (Player loc ((Destination point), []) curr next)
sameDir (Player loc ((Destination point), [t]) curr next) b 
  | nextTracks == Null = Player loc (Destination loc, [t]) curr curr 
  | otherwise = (Player loc (deconDestination nextTracks, [t] ++ deconTracks nextTracks) curr curr) 
  where 
    nextPiv = getPivot point b
    nextTracks = getTracks nextPiv curr
sameDir (Player loc ((Destination point), ts) curr next) b = (Player loc ((Destination point), ts) curr next)

closeEnough :: [a] -> Bool
closeEnough xs = length xs <=25

{-
this is called every frame; todo:
(1) change the position
-}
updatePlayer :: Player -> Board -> Player
updatePlayer p b = movePlayer p b
