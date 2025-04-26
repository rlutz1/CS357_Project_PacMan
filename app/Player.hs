{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
module Player where

-- import Brillo
-- -- import Board
-- import Data
-- import Utils






{-
------------------------------------------------------------
DRAW FUNCTIONS
-- ------------------------------------------------------------
-- -}





-- {-
-- ------------------------------------------------------------
-- UPDATE FUNCTIONS
-- ------------------------------------------------------------
-- -}

-- updatePlayer :: Player -> Board -> Player
-- updatePlayer = movePlayer

-- {-
-- ------------------------------------------------------------
-- PLAYER MOVEMENT FUNCTIONS
-- ------------------------------------------------------------
-- -}


-- {-
-- first, if the player currdir /= nextdir, check 
--   if they're at close enough to end of their track
--     grab the pivot (via player destination) from the board, try to grab the corresponding neighbor from that pivot
--     if its null
--       instead keep trying to go the same direction as before, can't turn there
--       if that is null, stop moving -- keep same location
-- want to clean this up, works well enough to continue testing, but there's def some gunk to cut out
-- -}
-- movePlayer :: Player -> Board -> Player
-- movePlayer (Player loc (dest, []) curr next d u coll) _ = Player loc (dest, [loc]) curr next d u coll
-- movePlayer (Player _ (dest, t:ts) curr next d u coll) b
--   | curr /= next && closeEnough ts = changeDir (Player t (dest, ts) curr next d u coll) b
--   | otherwise = sameDir (Player t (dest, ts) curr curr d u coll) b
    
-- -- queueTracks :: Player -> Board -> Player
-- -- queueTracks (Player loc (dest, ts) curr next) b -- we are attempting oto queue up the next move
-- --   | curr /= next = changeDir (Player loc (dest, ts) curr next) b 
-- --   | otherwise = sameDir (Player loc (dest, ts) curr next) b 

-- -- todo clean up
-- changeDir :: Player -> Board -> Player
-- changeDir (Player loc (Destination point, ts) curr next d u coll) b 
--   | nextTracks == Null = sameDir (Player loc (Destination point, ts) curr curr d u coll) b 
--   | otherwise = Player loc (deconDestination nextTracks, ts ++ deconTracks nextTracks) next next d u coll
--   where 
--     nextPiv = getPivot point b
--     nextTracks = getTracks nextPiv next



-- -- todo clean up
-- sameDir :: Player -> Board -> Player
-- sameDir (Player loc (Destination point, []) curr _ d u coll) _ = Player loc (Destination point, []) curr curr d u coll
-- sameDir (Player loc (Destination point, [t]) curr _ d u coll) b
--   | nextTracks == Null = Player loc (Destination point, [t]) curr curr d u coll
--   | otherwise = Player loc (deconDestination nextTracks, t :deconTracks nextTracks) curr curr d u coll
--   where 
--     nextPiv = getPivot point b
--     nextTracks = getTracks nextPiv curr
-- sameDir (Player loc (Destination point, ts) curr next d u coll) _ = Player loc (Destination point, ts) curr next d u coll 

-- closeEnough :: [a] -> Bool
-- closeEnough xs = length xs <= 25




