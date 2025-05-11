module Utils where

import Brillo
import Data

sameCol :: Point -> Point -> Bool
sameCol (x1, _) (x2, _) = x1 == x2

sameRow :: Point -> Point -> Bool
sameRow (_, y1) (_, y2) = y1 == y2

deconDest :: Neighbor -> Point
deconDest Null = error "what is wrong with you"
deconDest (Neighbor _ dest _) = dest 

getPiv :: Maybe Pivot -> Pivot
getPiv (Just pv) = pv
getPiv _ = error "don't give nothing please"  

getNeighborPoint :: Neighbor -> Point
getNeighborPoint (Neighbor _ dest _) = dest
getNeighborPoint _ = error "don't give this null, dummy"

getValidNeighbors :: Board -> Point -> [Neighbor]
getValidNeighbors b pt = filter (/= Null) [up,  right, left, down]
  where 
    (Pivot _ (up, down, left, right)) = getPiv (getPivot pt b)

getSpecificNeighbor :: Maybe Pivot -> Direction -> Neighbor
getSpecificNeighbor Nothing _ = Null
getSpecificNeighbor (Just (Pivot _ (upN, downN, leftN, rightN))) dir
  | dir == UP = upN
  | dir == DOWN = downN
  | dir == LEFT = leftN
  | dir == RIGHT = rightN
  | otherwise = Null

getPlayerLocation :: Board -> Point
getPlayerLocation (Board ts ps cs l s dB uB (Player loc _ _ _ _ _ _) gs gOver timers) = loc

getPlayerDestination :: Board -> Point
getPlayerDestination (Board ts ps cs l s dB uB (Player _ ( pt, _) _ _ _ _ _) gs gOver timers) = pt


defaultSpeed :: Float
defaultSpeed = 1 

fastSpeed :: Float
fastSpeed = 1.2

slowSpeed :: Float
slowSpeed = 0.8

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

getPivot :: Point -> Board -> Maybe Pivot
getPivot _ (Board _ [] _ _ _ _ _ _ _ _ _) = Nothing
getPivot point (Board ts ((Pivot pt ns):ps) cs l s d u p gs gOver timers)
  | point == pt = Just (Pivot pt ns)
  | otherwise = getPivot point (Board ts ps cs l s d u p gs gOver timers) 
