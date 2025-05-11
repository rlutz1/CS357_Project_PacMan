module Ghost where

import Brillo
import Data
import Utils
import Data.Word
import System.Random

{-
  @author Roxanne Lutz
  the ghosts and their specific algorithms and 
  functionalities, update, drawing, etc.
-}

{-
------------------------------------------------------------
GHOST UPDATE FUNCTIONS
------------------------------------------------------------
-}

-- general case update ghost with each frame function
updateGhosts :: [Ghost] -> Board -> [Ghost]
updateGhosts gs board = go [] gs board
  where 
    go acc [] _ = acc
    go acc ((Ghost name loc path d u inf):gs) board = go (u (Ghost name loc path d u inf) board : acc) gs board

-- function kept separate for updating Blinky in case other pieces wanted later. for now, just move
updateBlinky :: Ghost -> Board -> Ghost
updateBlinky = moveBlinky

-- function kept separate for updating Pinky in case other pieces wanted later. for now, just move
updatePinky :: Ghost -> Board -> Ghost
updatePinky = movePinky

-- function kept separate for updating Inky in case other pieces wanted later. for now, just move
updateInky :: Ghost -> Board -> Ghost
updateInky = moveInky

-- function kept separate for updating Clyde in case other pieces wanted later. for now, just move
updateClyde :: Ghost -> Board -> Ghost
updateClyde = moveClyde

-- function to generate all ghosts for the board
genGhosts :: [Ghost]
genGhosts = [genBlinky, genPinky, genInky, genClyde]

-- function to generate blinky
genBlinky :: Ghost
genBlinky 
  = Ghost "Blinky" blinkyStartPoint (blinkyStartPoint, []) (drawGhost blinkyDefColor) updateBlinky (uniforms (mkStdGen defaultBlinkySeed) :: [Int])

-- function to generate pinky
genPinky :: Ghost
genPinky 
  = Ghost "Pinky" pinkyStartPoint (pinkyStartPoint, []) (drawGhost pinkyDefColor) updatePinky (uniforms (mkStdGen defaultPinkySeed) :: [Int])

-- function to generate inky
genInky :: Ghost
genInky 
  = Ghost "Inky" inkyStartPoint (inkyStartPoint, []) (drawGhost inkyDefColor) updateInky (uniforms (mkStdGen defaultInkySeed) :: [Int])

-- function to generate clyde
genClyde :: Ghost
genClyde 
  = Ghost "Clyde" clydeStartPoint (clydeStartPoint, []) (drawGhost clydeDefColor) updateClyde (uniforms (mkStdGen defaultClydeSeed) :: [Int])

{-
------------------------------------------------------------
GHOST MOVEMENT FUNCTIONS
------------------------------------------------------------
-}

-- blinky is the nuke, but he doesn't move quite as fast as the player
moveBlinky :: Ghost -> Board -> Ghost
moveBlinky (Ghost name loc (point, []) d u inf) b 
  = Ghost name loc ( point, [loc]) d u inf
moveBlinky (Ghost name loc (point, [t]) d u inf) b 
  = Ghost name t (getPlayerDestination b, nuke slowSpeed b (getPlayerDestination b) (addPaths (getValidNeighbors b t) []) [point]) d u inf
moveBlinky (Ghost name loc (dest, t:ts) d u inf) _ = Ghost name t (dest, ts) d u inf

-- pinky, the nuke but only when player happens to be in same row/col when refilling, otherwise meanderer
movePinky :: Ghost -> Board -> Ghost
movePinky (Ghost name loc (point, []) d u inf) b 
  = Ghost name loc (point, [loc]) d u inf
movePinky (Ghost name (x, y) (point, [t]) d u (i:inf)) b
  | sameCol (getPlayerDestination b) (x, y) || sameRow (getPlayerDestination b) (x, y) 
    = Ghost name t ((getPlayerDestination b), nuke defaultSpeed b (getPlayerDestination b) (addPaths (getValidNeighbors b t) []) [point]) d u inf
  | otherwise = Ghost name t (getPlayerDestination b, meander defaultSpeed i b (giveRandomNeighbor (getValidNeighbors b t) i) [point] []) d u inf
movePinky (Ghost name loc (point, t:ts) d u inf) b = Ghost name t (point, ts) d u inf

-- inky, a fast meanderer
moveInky :: Ghost -> Board -> Ghost
moveInky (Ghost name loc (point, []) d u inf) b 
  = Ghost name loc (point, [loc]) d u inf
moveInky (Ghost name (x, y) (point, [t]) d u (i:inf)) b 
  = Ghost name t (getPlayerDestination b, meander fastSpeed i b (giveRandomNeighbor (getValidNeighbors b t) i) [point] []) d u inf
moveInky (Ghost name loc (dest, t:ts) d u inf) _ = Ghost name t (dest, ts) d u inf

-- clyde, a meanderer
moveClyde :: Ghost -> Board -> Ghost
moveClyde (Ghost name loc (point, []) d u inf) b 
  = Ghost name loc (point, [loc]) d u inf
moveClyde (Ghost name (x, y) ( point, [t]) d u (i:inf)) b 
  = Ghost name t (getPlayerDestination b, meander defaultSpeed i b (giveRandomNeighbor (getValidNeighbors b t) i) [point] []) d u inf
moveClyde (Ghost name loc (dest, t:ts) d u inf) _ = Ghost name t (dest, ts) d u inf

-- simply build a path through the board until you cannot go any further. dfs-like.
meander :: Float -> Int -> Board -> Neighbor -> [Point] -> [Track] -> [Track]
meander speed seed b (Neighbor dir dest from) visited path 
  | null unvisited = path
  | otherwise =  meander speed seed b (giveRandomNeighbor unvisited seed) (dest:visited) (path ++ tracksToNeighbor)
  where 
    next = getValidNeighbors b dest
    unvisited = getUnvisited next visited
    tracksToNeighbor = genTracks speed dir from dest

-- find the shortest path to the player. it is a bfs shortest path algo.
nuke :: Float -> Board -> Point -> [(Neighbor, [Track])] ->  [Point] -> [Track]
nuke speed b ultimateDest ((Neighbor dir dest from, path):queue) visited 
  | dest == ultimateDest = path ++ tracksToNeighbor
  | otherwise = 
    if dest `elem` visited 
      then nuke speed b ultimateDest (queue) (visited) 
      else  nuke speed b ultimateDest (queue ++ nextsWithPaths) (dest:visited)
  where
    next = getValidNeighbors b dest 
    tracksToNeighbor = genTracks speed dir from dest
    nextsWithPaths = addPaths next (path ++ tracksToNeighbor)

-- utility for keeping track of paths created for bfs
addPaths :: [Neighbor] -> [Track] -> [(Neighbor, [Track])]
addPaths [] path = []
addPaths (n:ns) path = (n, path) : addPaths ns path

-- get all unvisited neighbors. essentially filter with some extra guards.
getUnvisited :: [Neighbor] -> [Point] -> [Neighbor]
getUnvisited [] _ = []
getUnvisited ((Neighbor dir dest from):ns) vis 
  | dest `elem` vis = getUnvisited ns vis
  | otherwise = (Neighbor dir dest from) : getUnvisited ns vis

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

-- main method to draw all ghosts on board
drawGhosts :: [Ghost] -> [Picture]
drawGhosts gs = go [] gs
  where 
    go acc [] = acc
    go acc ((Ghost name loc path d u inf):gs) = go (d (Ghost name loc path d u inf) : acc) gs 

-- draw an individual ghost
drawGhost :: Color -> Ghost -> Picture
drawGhost c (Ghost _ (x, y) _ _ _ _) = color c (translate x y (thickCircle 10 20))

-- diff draw function if collision detection turned off
drawGhostsOff :: [Ghost] -> [Ghost]
drawGhostsOff [] = []
drawGhostsOff ((Ghost name locG desG _ uG inf):gs) = Ghost name locG desG (drawGhost white) uG inf : drawGhostsOff gs

-- diff draw function if collision detection turned on
drawGhostsOn :: [Ghost] -> [Ghost]
drawGhostsOn  [] = []
drawGhostsOn  ((Ghost name locG desG _ uG inf):gs)
    | name == "Blinky" = Ghost name locG desG (drawGhost blinkyDefColor) uG inf : drawGhostsOn gs
    | name == "Inky"   = Ghost name locG desG (drawGhost inkyDefColor) uG inf : drawGhostsOn gs
    | name == "Pinky"  = Ghost name locG desG (drawGhost pinkyDefColor) uG inf : drawGhostsOn gs
    | otherwise        = Ghost name locG desG (drawGhost clydeDefColor) uG inf : drawGhostsOn gs

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

defaultBlinkySeed :: Int
defaultBlinkySeed = 42

defaultPinkySeed :: Int
defaultPinkySeed = 79

defaultInkySeed :: Int
defaultInkySeed = 7

defaultClydeSeed :: Int
defaultClydeSeed = 227

blinkyDefColor :: Color
blinkyDefColor = red

pinkyDefColor :: Color
pinkyDefColor = makeColor 0.898 0.388 0.780 1 

inkyDefColor :: Color
inkyDefColor = makeColor 0.262 0.788 0.88 1

clydeDefColor :: Color
clydeDefColor = makeColor 0.871 0.675 0.243 1 