{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
module Player where

import Brillo
import Board




genPlayer :: Player 
genPlayer = Player playerStartPoint (Destination playerStartPoint, []) NONE NONE

{-
this is called every frame; todo:
(1) change the position
-}
updatePlayer :: Player -> Board -> Player
updatePlayer = movePlayer

{-
------------------------------------------------------------
MOVEMENT FUNCTIONS
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
movePlayer (Player loc (dest, []) curr next) _ = Player loc (dest, [loc]) curr next
movePlayer (Player _ (dest, t:ts) curr next) b
  | curr /= next && closeEnough ts = changeDir (Player t (dest, ts) curr next) b
  | otherwise = sameDir (Player t (dest, ts) curr curr) b
    
-- queueTracks :: Player -> Board -> Player
-- queueTracks (Player loc (dest, ts) curr next) b -- we are attempting oto queue up the next move
--   | curr /= next = changeDir (Player loc (dest, ts) curr next) b 
--   | otherwise = sameDir (Player loc (dest, ts) curr next) b 

-- todo clean up
changeDir :: Player -> Board -> Player
changeDir (Player loc (Destination point, ts) curr next) b 
  | nextTracks == Null = sameDir (Player loc (Destination point, ts) curr curr) b 
  | otherwise = Player loc (deconDestination nextTracks, ts ++ deconTracks nextTracks) next next
  where 
    nextPiv = getPivot point b
    nextTracks = getTracks nextPiv next



-- todo clean up
sameDir :: Player -> Board -> Player
sameDir (Player loc (Destination point, []) curr _) _ = Player loc (Destination point, []) curr curr
sameDir (Player loc (Destination point, [t]) curr _) b
  | nextTracks == Null = Player loc (Destination point, [t]) curr curr 
  | otherwise = Player loc (deconDestination nextTracks, t :deconTracks nextTracks) curr curr
  where 
    nextPiv = getPivot point b
    nextTracks = getTracks nextPiv curr
sameDir (Player loc (Destination point, ts) curr next) _ = Player loc (Destination point, ts) curr next

closeEnough :: [a] -> Bool
closeEnough xs = length xs <= 25



{-
POTENTIAL CLEANUP, a little janky, need to to not queue so persistently, but a start

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
movePlayer (Player loc (dest, []) curr next) _ = Player loc (dest, [loc]) curr next
movePlayer (Player _ (dest, t:ts) curr next) b
  | curr /= next && closeEnough ts = changeDir (Player t (dest, ts) curr next) b
  | curr == next && needRefill ts = sameDir (Player t (dest, ts) curr curr) b
  | otherwise = Player t (dest, ts) curr next
    


-- queueTracks :: Player -> Board -> Player
-- queueTracks (Player loc (dest, ts) curr next) b -- we are attempting oto queue up the next move
--   | curr /= next = changeDir (Player loc (dest, ts) curr next) b 
--   | otherwise = sameDir (Player loc (dest, ts) curr next) b 

-- todo clean up
changeDir :: Player -> Board -> Player
changeDir (Player loc (Destination point, ts) curr next) b 
  | nextTracks == Null = Player loc (Destination point, ts) curr curr
  | otherwise = Player loc (deconDestination nextTracks, ts ++ deconTracks nextTracks) next next
  where 
    nextPiv = getPivot point b
    nextTracks = getTracks nextPiv next



-- todo clean up
sameDir :: Player -> Board -> Player
sameDir (Player loc (Destination point, ts) curr _) b 
  | nextTracks == Null = Player loc (Destination point, ts) curr curr 
  | otherwise = Player loc (deconDestination nextTracks, ts ++ deconTracks nextTracks) curr curr
  where 
    nextPiv = getPivot point b
    nextTracks = getTracks nextPiv curr

-- sameDir (Player loc (Destination point, []) curr _) _ = Player loc (Destination point, []) curr curr
-- sameDir (Player loc (Destination point, [t]) curr _) b

-- sameDir (Player loc (Destination point, ts) curr next) _ = Player loc (Destination point, ts) curr next

closeEnough :: [a] -> Bool
closeEnough xs = length xs <= 25

needRefill :: [a] -> Bool
needRefill [] = True
needRefill [_] = True
needRefill _ = False

-}

