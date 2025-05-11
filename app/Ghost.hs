module Ghost where

import Brillo
import Data
import Utils
import Data.Word
import System.Random

{-
------------------------------------------------------------
GHOST UPDATE FUNCTIONS
------------------------------------------------------------
-}

updateGhosts :: [Ghost] -> Board -> [Ghost]
updateGhosts gs board = go [] gs board--foldr go [] gs --go gs board []
  where 
    go acc [] _ = acc
    go acc ((Ghost name loc path curr next d u inf):gs) board = go (u (Ghost name loc path curr next d u inf) board : acc) gs board

updateBlinky :: Ghost -> Board -> Ghost
updateBlinky g board = moveBlinky g board

updatePinky :: Ghost -> Board -> Ghost
updatePinky g board = movePinky g board

updateInky :: Ghost -> Board -> Ghost
updateInky g board = moveInky g board

updateClyde :: Ghost -> Board -> Ghost
updateClyde g board = moveClyde g board

genGhosts :: [Ghost]
genGhosts = [genBlinky, genPinky, genInky, genClyde]
-- Blinky, Pinky, Inky and Clyde,
genBlinky :: Ghost
genBlinky = Ghost "Blinky" blinkyStartPoint ( blinkyStartPoint, []) NONE NONE (drawGhost blinkyDefColor) updateBlinky (uniforms (mkStdGen 42) :: [Int])

genPinky :: Ghost
genPinky = Ghost "Pinky" pinkyStartPoint ( pinkyStartPoint, []) NONE NONE (drawGhost pinkyDefColor) updatePinky (uniforms (mkStdGen 79) :: [Int])

genInky :: Ghost
genInky = Ghost "Inky" inkyStartPoint ( inkyStartPoint, []) NONE NONE (drawGhost inkyDefColor) updateInky (uniforms (mkStdGen 7) :: [Int])

genClyde :: Ghost
genClyde = Ghost "Clyde" clydeStartPoint ( clydeStartPoint, []) NONE NONE (drawGhost clydeDefColor) updateClyde (uniforms (mkStdGen 782) :: [Int])

blinkyDefColor :: Color
blinkyDefColor = red

-- rgb(229, 99, 199) 
pinkyDefColor :: Color
pinkyDefColor = makeColor 0.898 0.388 0.780 1 

-- rgb(67, 201, 225) 
inkyDefColor :: Color
inkyDefColor = makeColor 0.262 0.788 0.88 1

-- rgb(222, 174, 62) 
clydeDefColor :: Color
clydeDefColor = makeColor 0.871 0.675 0.243 1 


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


-- data Neighbor = Null | Neighbor Direction Destination From
meander :: Int -> Board -> Neighbor -> [Point] -> [Track] -> [Track]
meander seed b  (Neighbor dir dest from) visited path 
  | null unvisited = path
  | otherwise =  meander seed b (giveRandomNeighbor unvisited seed) (dest:visited) (path ++ tracksToNeighbor)
  where 
    next = getValidNeighbors b dest
    unvisited = getUnvisited next visited
    tracksToNeighbor = genTracks (1.0) dir from dest

getUnvisited :: [Neighbor] -> [Point] -> [Neighbor]
getUnvisited [] _ = []
getUnvisited ((Neighbor dir dest from):ns) vis 
  | dest `elem` vis = getUnvisited ns vis
  | otherwise = (Neighbor dir dest from) : getUnvisited ns vis

-- (genTracks defaultSpeed UP (x, y) (x, y + 50)
--0.625
nukeButSlow :: Board -> Point -> [(Neighbor, [Track])] ->  [Point] -> [Track]
nukeButSlow b ultimateDest ((Neighbor dir dest from, path):queue) visited 
  | dest == ultimateDest = path ++ tracksToNeighbor
  | otherwise = 
    if dest `elem` visited 
      then nukeButSlow b ultimateDest (queue) (visited) 
      else  nukeButSlow b ultimateDest (queue ++ nextsWithPaths) (dest:visited)
      -- if not (null recur) then prev ++ recur else recur
  -- | otherwise = tracksToNeighbor ++ recur
  where
    next = getValidNeighbors b dest
    tracksToNeighbor = genTracks (0.8) dir from dest
    nextsWithPaths = addPaths next (path ++ tracksToNeighbor)
    

    -- recur = nuke order b dest (queue ++ next) (pt:visited) tracksToNeighbor

nuke :: Board -> Point -> [(Neighbor, [Track])] ->  [Point] -> [Track]
nuke  b ultimateDest ((Neighbor dir dest from, path):queue) visited 
  | dest == ultimateDest = path ++ tracksToNeighbor
  | otherwise = 
    if dest `elem` visited 
      then nuke  b ultimateDest (queue) (visited) 
      else  nuke  b ultimateDest (queue ++ nextsWithPaths) (dest:visited)

  where
    next = getValidNeighbors b dest 
    tracksToNeighbor = genTracks (1.0) dir from dest
    nextsWithPaths = addPaths next (path ++ tracksToNeighbor)

addPaths :: [Neighbor] -> [Track] -> [(Neighbor, [Track])]
addPaths [] path = []
addPaths (n:ns) path = (n, path) : addPaths ns path


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

drawGhosts :: [Ghost] -> [Picture]
drawGhosts gs = go [] gs--foldr go [] gs --go gs board []
  where 
    go acc [] = acc
    go acc ((Ghost name loc path curr next d u inf):gs) = go (d (Ghost name loc path curr next d u inf) : acc) gs 

drawGhost :: Color -> Ghost -> Picture
drawGhost c (Ghost _ (x, y) _ _ _ _ _ _) = color c (translate x y (thickCircle 10 20))

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

{-
------------------------------------------------------------
GHOST CONSTANTS
------------------------------------------------------------
-}

blinkyStartPoint :: Point
blinkyStartPoint = (475, 225)

pinkyStartPoint :: Point
pinkyStartPoint = (475, -475)

inkyStartPoint :: Point
inkyStartPoint = (-475, 225)

clydeStartPoint :: Point
clydeStartPoint = (75, 25)