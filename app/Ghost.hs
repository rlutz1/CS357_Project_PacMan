module Ghost where

import Brillo
import Data
import Utils
import Data.Word
import System.Random


{-
------------------------------------------------------------
GHOST MOVEMENT FUNCTIONS
------------------------------------------------------------
-}

-- the nuke
moveBlinky :: Ghost -> Board -> Ghost
moveBlinky (Ghost name loc ( point, []) curr next d u inf) b 
  = Ghost name loc ( point, [loc]) curr next d u inf
moveBlinky (Ghost name loc ( point, [t]) curr next d u inf) b 
  = Ghost name t (getPlayerDestination b, nukeButSlow b (getPlayerDestination b) (addPaths (getValidNeighbors b t) []) [point]) curr next d u inf
moveBlinky (Ghost name loc (dest, t:ts) curr next d u inf) _ = Ghost name t (dest, ts) curr next d u inf

-- the nuke but only when player happens to be in same row/col when refilling, otherwise meanderer
movePinky :: Ghost -> Board -> Ghost
movePinky (Ghost name loc (point, []) curr next d u inf) b 
  = Ghost name loc (point, [loc]) curr next d u inf
movePinky (Ghost name (x, y) (point, [t]) curr next d u (i:inf)) b
  | sameCol (getPlayerDestination b) (x, y) || sameRow (getPlayerDestination b) (x, y) = Ghost name t ((getPlayerDestination b), nuke b (getPlayerDestination b) (addPaths (getValidNeighbors b t) []) [point]) curr next d u inf
  | otherwise = Ghost name t (getPlayerDestination b, meander i b (giveRandomNeighbor (getValidNeighbors b t) i) [point] []) curr next d u inf
movePinky (Ghost name loc (point, t:ts) curr next d u inf) b = Ghost name t (point, ts) curr next d u inf

-- a meanderer
moveInky :: Ghost -> Board -> Ghost
moveInky (Ghost name loc (point, []) curr next d u inf) b 
  = Ghost name loc (point, [loc]) curr next d u inf
moveInky (Ghost name (x, y) (point, [t]) curr next d u (i:inf)) b 
  = Ghost name t (getPlayerDestination b, meander i b (giveRandomNeighbor (getValidNeighbors b t) i) [point] []) curr next d u inf
moveInky (Ghost name loc (dest, t:ts) curr next d u inf) _ = Ghost name t (dest, ts) curr next d u inf

-- a meanderer
moveClyde :: Ghost -> Board -> Ghost
moveClyde (Ghost name loc (point, []) curr next d u inf) b 
  = Ghost name loc (point, [loc]) curr next d u inf
moveClyde (Ghost name (x, y) ( point, [t]) curr next d u (i:inf)) b 
  = Ghost name t (getPlayerDestination b, meander i b (giveRandomNeighbor (getValidNeighbors b t) i) [point] []) curr next d u inf
moveClyde (Ghost name loc (dest, t:ts) curr next d u inf) _ = Ghost name t (dest, ts) curr next d u inf

-- function to "randomly" shuffle the order at which we get back the list of neighbors back as
giveRandomNeighbor :: [Neighbor] -> Int -> Neighbor
giveRandomNeighbor [] _ = Null
giveRandomNeighbor ns seed = head shuffled
  where
    shuffled = fst (uniformShuffleList ns (mkStdGen seed))

{-
------------------------------------------------------------
GHOST DRAWING FUNCTIONS
------------------------------------------------------------
-}

drawGhostsOff :: [Ghost] -> [Ghost]
drawGhostsOff [] = []
drawGhostsOff ((Ghost name locG desG currG nextG _ uG inf):gs) = Ghost name locG desG currG nextG (drawGhost white) uG inf : drawGhostsOff gs

drawGhostsOn :: [Ghost] -> [Ghost]
drawGhostsOn  [] = []
drawGhostsOn  ((Ghost name locG desG currG nextG _ uG inf):gs)
    | name == "Blinky" = Ghost name locG desG currG nextG (drawGhost blinkyDefColor) uG inf : drawGhostsOn gs
    | name == "Inky"   = Ghost name locG desG currG nextG (drawGhost inkyDefColor) uG inf : drawGhostsOn gs
    | name == "Pinky"  = Ghost name locG desG currG nextG (drawGhost pinkyDefColor) uG inf : drawGhostsOn gs
    | otherwise        = Ghost name locG desG currG nextG (drawGhost clydeDefColor) uG inf : drawGhostsOn gs