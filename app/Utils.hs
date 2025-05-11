module Utils where

import Brillo
import Data

{-
  @author Roxanne Lutz
  general utilities common to many pieces of the game. 
  things like path finding aspects needed by both
  player and ghosts.
  nicer to separate them out entirely so all can access.
-}

-- test if points are in same column
sameCol :: Point -> Point -> Bool
sameCol (x1, _) (x2, _) = x1 == x2

-- test if points are in same row
sameRow :: Point -> Point -> Bool
sameRow (_, y1) (_, y2) = y1 == y2

-- deconstruct the pivot from a maybe piv
getPiv :: Maybe Pivot -> Pivot
getPiv (Just pv) = pv
getPiv _ = error "don't give nothing please"  

-- getter for deconstructing neighbor's point
getNeighborPoint :: Neighbor -> Point
getNeighborPoint (Neighbor _ dest _) = dest
getNeighborPoint _ = error "don't give this null, dummy"

-- filter out any null neighbors
getValidNeighbors :: Board -> Point -> [Neighbor]
getValidNeighbors b pt = filter (/= Null) [up,  right, left, down]
  where 
    (Pivot _ (up, down, left, right)) = getPiv (getPivot pt b)

-- get a neighbor in a specific direction
getSpecificNeighbor :: Maybe Pivot -> Direction -> Neighbor
getSpecificNeighbor Nothing _ = Null
getSpecificNeighbor (Just (Pivot _ (upN, downN, leftN, rightN))) dir
  | dir == UP = upN
  | dir == DOWN = downN
  | dir == LEFT = leftN
  | dir == RIGHT = rightN
  | otherwise = Null

-- deconstruct the player destination (where they're headed)
getPlayerDestination :: Board -> Point
getPlayerDestination (Board ts ps cs l s dB uB (Player _ ( pt, _) _ _ _ _ _) gs gOver timers) = pt

-- generate tracks to a point. essentially, queue up a series of movements in the direction of a point
genTracks :: Float -> Direction -> Point -> Point -> [Track]
genTracks speed dir start end
  | dir == UP = go (>=) start end [] 0 speed
  | dir == DOWN = go (<=) start end [] 0 (-speed)
  | dir == LEFT = go (<=) start end [] (-speed) 0
  | dir == RIGHT = go (>=) start end [] speed 0
  | otherwise = []
  where 
    go pred (x1, y1) (x2, y2) acc xAcc yAcc
      | pred x1 x2 && pred y1 y2 = reverse ((x2, y2) : acc) -- issue with works for up/right i think, potential issue down left
      | otherwise = go pred (x1 + xAcc, y1 + yAcc) (x2, y2) ((x1 + xAcc, y1 + yAcc) : acc) xAcc yAcc

-- get a pivot given a specified point
getPivot :: Point -> Board -> Maybe Pivot
getPivot _ (Board _ [] _ _ _ _ _ _ _ _ _) = Nothing
getPivot point (Board ts ((Pivot pt ns):ps) cs l s d u p gs gOver timers)
  | point == pt = Just (Pivot pt ns)
  | otherwise = getPivot point (Board ts ps cs l s d u p gs gOver timers) 

-- constant for default speed
defaultSpeed :: Float
defaultSpeed = 1 

-- constant for faster speed
fastSpeed :: Float
fastSpeed = 1.2

-- constant for slower speed
slowSpeed :: Float
slowSpeed = 0.8
