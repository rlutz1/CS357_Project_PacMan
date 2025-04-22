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
-- movePlayer (Player loc (dest, []) curr next) = undefined -- no moves left
movePlayer (Player loc (dest, []) curr next) b = (Player loc (dest, [loc]) curr next)
movePlayer (Player loc (dest, [p]) curr next) b -- exactly one move left queued
  | curr /= next = changeDir (Player loc (dest, [p]) curr next) b 
  | otherwise = sameDir (Player loc (dest, [p]) curr next) b 
movePlayer (Player loc (dest, (t:ts)) curr next) _ = (Player t (dest, (ts)) curr curr)

changeDir :: Player -> Board -> Player
-- changeDir (Player loc (None, [t]) curr next) b = (Player t (None, []) curr curr)
changeDir (Player loc ((Destination point), [t]) curr next) b 
  | nextTracks == Null = sameDir (Player loc ((Destination point), [t]) curr curr) b 
  | otherwise = (Player t (deconDestination nextTracks, deconTracks nextTracks) next next)
  where 
    nextPiv = getPivot point b
    nextTracks = getTracks nextPiv next


sameDir :: Player -> Board -> Player
-- sameDir (Player loc (None, [t]) curr next) b = (Player t (None, []) curr curr)
sameDir (Player loc ((Destination point), [t]) curr next) b 
  | nextTracks == Null = Player t (Destination t, []) NONE NONE 
  | otherwise = (Player t (deconDestination nextTracks, deconTracks nextTracks) curr curr) 
  where 
    nextPiv = getPivot point b
    nextTracks = getTracks nextPiv curr

{-
this is called every frame; todo:
(1) change the position
-}
updatePlayer :: Player -> Board -> Player
updatePlayer p b = movePlayer p b
