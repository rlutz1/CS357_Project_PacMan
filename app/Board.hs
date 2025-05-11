{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
module Board where

import Brillo
import Data
import Utils
import Ghost
import Player


{-
------------------------------------------------------------
BOARD UPDATE FUNCTIONS
------------------------------------------------------------
-}

-- general case main update board function, called each frame
updateBoard :: Board -> Float -> Board
updateBoard (Board ts ps cs ls s dB uB p gs gOver timers) dt = 
  do
    let updatedP = updatePlayer p (Board ts ps cs ls s dB uB p gs gOver timers)
    let updatedGs = updateGhosts gs (Board ts ps cs ls s dB uB p gs gOver timers)
    let updatedColls = updateCollectibles (Board ts ps cs ls s dB uB updatedP updatedGs gOver timers)
    let updatedEffects = updateEffects updatedColls dt []
    checkCollision (updatedEffects) updatedP updatedGs

checkCollision :: Board -> Player -> [Ghost] -> Board
checkCollision b p [] = b
checkCollision 
  (Board ts ps cs lives s dB uB op ogs gOver timers)
  (Player locP desP currP nextP dP uP coll) 
  ((Ghost name locG desG currG nextG dG uG inf):gs)
    | coll = if collisionDetected locP locG then kill (Board ts ps cs lives s dB uB op ogs gOver timers) else checkCollision (Board ts ps cs lives s dB uB op ogs gOver timers) (Player locP desP currP nextP dP uP coll) gs
    | otherwise = Board ts ps cs lives s dB uB op ogs gOver timers

-- kill the pacman! he ran into a ghost!
kill :: Board -> Board
kill (Board ts ps cs lives s dB uB op ogs gOver timers) 
  | checkGameOver (lives - 1) = Board ts ps cs lives s drawGameLost (\b f -> b) op ogs True timers
  | otherwise = Board ts ps cs (lives - 1) s dB uB genPlayer genGhosts gOver timers

-- update the collectibles list as needed as player roams the board
updateCollectibles :: Board -> Board 
updateCollectibles (Board ts ps cs l s d u (Player loc dest curr next dp up collDetect) gs gOver timers) 
  | length filteredColls < length cs = 
    enactEffect filteredColls (findColl loc cs) (Board ts ps cs l s d u (Player loc dest curr next dp up collDetect) gs gOver timers) 
  | otherwise = 
    if allEaten cs 
      then Board ts ps cs l s drawGameWon (\b f -> b) (Player loc dest curr next dp up collDetect) gs True timers
      else Board ts ps cs l s d u (Player loc dest curr next dp up collDetect) gs gOver timers
  where
    filteredColls = filter (\c -> deconCollLoc c /= loc) cs

-- helper to filter through each potential "effect" on the board and make updates as needed, esp to timers
enactEffect :: [Collectible] -> Maybe Collectible ->  Board -> Board
enactEffect 
  filteredColls -- filtered out the eaten coll
  (Just (Collectible (GhostsOff time) score color' pos)) -- ghosts off effect
  (Board ts ps cs l s d u (Player loc dest curr next dp up collDetect) gs gOver timers) =
    Board ts ps filteredColls l (s + score) d u (Player loc dest curr next dp up False) (drawGhostsOff gs) gOver (addToTimers (GhostsOff time) timers)
enactEffect 
  filteredColls -- filtered out the eaten coll
  (Just (Collectible _ score color' pos)) -- nothing to do here
  (Board ts ps cs l s d u (Player loc dest curr next dp up collDetect) gs gOver timers) =
    Board ts ps filteredColls l (s + score) d u (Player loc dest curr next dp up collDetect) gs gOver timers
enactEffect 
  _ Nothing b = b -- this hypothetically should never happen, but here anyway

-- update the timers as needed, but if we've gone past the time limit, remove the timer and reverse any effects as needed
updateEffects :: Board -> Float -> [(Effect, Float, Float)] -> Board
updateEffects (Board ts ps cs lives s dB uB op ogs gOver []) dt acc = (Board ts ps cs lives s dB uB op ogs gOver acc)
updateEffects (Board ts ps cs lives s dB uB op ogs gOver ((eff, timer, end):timers)) dt acc
  | timesUp (eff, timer, end) = updateEffects (removeEffect (Board ts ps cs lives s dB uB op ogs gOver timers) (eff, timer, end)) dt acc
  | otherwise = updateEffects (Board ts ps cs lives s dB uB op ogs gOver timers) dt ((eff, timer + dt , end):acc)

-- function for adding an effect timer OR resetting if the effect is already active.
addToTimers :: Effect -> [(Effect, Float, Float)] -> [(Effect, Float, Float)]
addToTimers NoEffect ts = ts
addToTimers effect ts = go effect ts
  where 
    go eff [] = [(effect, 0, effectTime eff)]
    go eff ((e, runtimetime, end):ts) 
      | eff == e = (e, 0, end):ts
      | otherwise = (e, runtimetime, end) : go eff ts

-- remove any effects from the board, time is up
removeEffect :: Board -> (Effect, Float, Float) -> Board
removeEffect (Board ts ps cs lives s dB uB op ogs gOver timers) (NoEffect, _, _) = (Board ts ps cs lives s dB uB op ogs gOver timers)
removeEffect (Board ts ps cs lives s dB uB (Player locP desP currP nextP dP uP coll) ogs gOver timers) ((GhostsOff _), _, _)
  = Board ts ps cs lives s dB uB (Player locP desP currP nextP dP uP True) (drawGhostsOn ogs) gOver timers

-- simple helper function to test if timer is up
timesUp :: (Effect, Float, Float) -> Bool
timesUp (_, timer, end) = timer >= end

-- simple helper function to see if pacman has run out of lives
checkGameOver :: Int -> Bool
checkGameOver x = x < 0

-- deconstructor to grab the time allotted for the effect
effectTime :: Effect -> Float
effectTime (GhostsOff time) = time
effectTime NoEffect = 0

-- collision detection: are you within the defined limit of a ghost; number considers center -> center distance of circle center
collisionDetected :: Point -> Point -> Bool
collisionDetected (x1, y1) (x2, y2)
  | sameCol (x1, y1) (x2, y2) = max y1 y2 - min y1 y2 < 40
  | sameRow (x1, y1) (x2, y2) = max x1 x2 - min x1 x2 < 40
  | otherwise = False

-- little helper function for readability, simply testing if null collectible list
allEaten :: [Collectible] -> Bool
allEaten [] = True
allEaten _ = False

-- deconstruct the points from the collectible and return
updateScore :: Maybe Collectible -> Int
updateScore Nothing = 0
updateScore (Just (Collectible _ s _ _)) = s

-- see if there is a collectible at the players current location
findColl :: Point -> [Collectible] -> Maybe Collectible
findColl p [] = Nothing
findColl p ((Collectible e s c pt):cs)
  | p == pt = Just (Collectible e s c pt)
  | otherwise = findColl p cs

-- deconstructor for the collectible location
deconCollLoc :: Collectible -> Point
deconCollLoc (Collectible _ _ _ p) = p

{-
------------------------------------------------------------
BOARD DRAWING FUNCTIONS
------------------------------------------------------------
-}
drawGameWon :: Board -> [Picture]
drawGameWon b =  drawBoard b ++ drawGameWonNotification

drawGameLost :: Board -> [Picture]
drawGameLost b =  drawBoard b ++ drawGameLostNotification

-- rgb(157, 157, 157)
drawGameWonNotification :: [Picture]
drawGameWonNotification = [
  color (makeColor 0.616 0.616 0.616 1) (rectangleSolid 750 400),
  scale 0.25 0.25 (translate (-350) (200) (Text "YOU WON!")),
  scale 0.25 0.25 (translate (-1250) (0) (Text "You did escape the spooky ghosts!")),
  scale 0.25 0.25 (translate (-800) (-200) (Text "Press G to back to menu"))
  ]

-- rgb(157, 157, 157)
drawGameLostNotification :: [Picture]
drawGameLostNotification = [
  color (makeColor 0.616 0.616 0.616 1) (rectangleSolid 750 400),
  scale 0.25 0.25 (translate (-400) (200) (Text "YOU FAILED!")),
  scale 0.25 0.25 (translate (-1250) (0) (Text "You did not escape the ghosts.")),
  scale 0.25 0.25 (translate (-800) (-200) (Text "Press G to back to menu"))
  ]



{-
------------------------------------------------------------
TILES

handlers for the graphic representation of the tile map.
------------------------------------------------------------
-}

genTiles :: [Point] -> [Tile]
genTiles walls = [ Tile (getTileColor walls (x + 25, y + 25)) (Boundary y (y + 50) x (x + 50)) | x <- [-500, -450..450], y <- [-500, -450..200] ]

getTileColor :: [Point] -> Point -> Color
getTileColor walls center
  | elem center walls = black
  | otherwise = blue


{-
------------------------------------------------------------
TRACKS

handlers for the underlying movement mechanic, known as tracks.
------------------------------------------------------------
-}

-- up down left right
-- data Pivot = Pivot Point (Neighbor, Neighbor, Neighbor, Neighbor)
-- data Neighbor = Null | Neighbor Direction Destination From

genPivots :: [Point] -> [Pivot]
genPivots walls = [ Pivot p (getNeighbor UP p walls, getNeighbor DOWN p walls, getNeighbor LEFT p walls, getNeighbor RIGHT p walls) | p <- genCenters, notElem p walls]

genCollectibles :: [Point] -> [Collectible] -> [Collectible] -- for now, to get some basic ones "installed"
genCollectibles walls specials = specials ++ [ Collectible NoEffect 10 orange p | p <- genCenters, notElem p walls, notElem p specialPoints]
  where
    specialPoints = [ pt | (Collectible _ _ _ pt) <- specials ]

genCenters :: [Point]
genCenters = [ (x, y) | x <- [-475, -425..475], y <- [-475, -425..225] ]

  -- | dir == UP = if outOfBounds (x, y + 50) || elem (x, y + 50) walls then Null else Neighbor dir (x, y + 50) (genTracks defaultSpeed UP (x, y) (x, y + 50))
  -- | dir == DOWN = if outOfBounds (x, y - 50) || elem (x, y - 50) walls then Null else Neighbor dir ( (x, y - 50)) (genTracks defaultSpeed DOWN (x, y) (x, y - 50))
  -- | dir == LEFT = if outOfBounds (x - 50, y) || elem (x - 50, y) walls then Null else Neighbor dir ( (x - 50, y)) (genTracks defaultSpeed LEFT (x, y) (x - 50, y))
  -- | dir == RIGHT = if outOfBounds (x + 50, y) || elem (x + 50, y) walls then Null else Neighbor dir ( (x + 50, y)) (genTracks defaultSpeed RIGHT (x, y) (x + 50, y))
getNeighbor :: Direction -> Point -> [Point] -> Neighbor
getNeighbor dir (x, y) walls 
  | dir == UP = if outOfBounds (x, y + 50) || elem (x, y + 50) walls then Null else Neighbor dir (x, y + 50) (x, y)
  | dir == DOWN = if outOfBounds (x, y - 50) || elem (x, y - 50) walls then Null else Neighbor dir (x, y - 50) (x, y)
  | dir == LEFT = if outOfBounds (x - 50, y) || elem (x - 50, y) walls then Null else Neighbor dir (x - 50, y) (x, y)
  | dir == RIGHT = if outOfBounds (x + 50, y) || elem (x + 50, y) walls then Null else Neighbor dir (x + 50, y) (x, y)
  | otherwise = Null

defaultSpeed :: Float
defaultSpeed = 1 -- 1 px / frame

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

genPlayer :: Player 
genPlayer = Player playerStartPoint ( playerStartPoint, []) NONE NONE drawPlayer updatePlayer False

{-
------------------------------------------------------------
WALLS

specify walls by the center point only.
------------------------------------------------------------
-}

-- lvl1Walls :: [Point]
-- lvl1Walls =  [
--   (-375, -375), 
--   (-375, -325), 
--   (-375, -275),
--   (-375, -225),

--   (-375, -125),
--   (-375, -75),
--   (-375, -25)
--   ]

lvl1Walls :: [Point]
lvl1Walls =  [(-375, -375), 
              (-375, -325), 
              (-375, -275),
              (-375, -225),

              (-375, -125),
              (-375, -75),
              (-375, -25),
              (-375, 25),

              (-325, -275),
              (-325, -75),

              (-275, -375), 
              (-275, -325), 
              (-275, -275),
              (-275, -225),

              (-275, -125),
              (-275, -75),
              (-275, -25),
              (-275, 25),

              (-225, -275),
              (-225, -75),

              (-175, -375), 
              (-175, -325), 
              (-175, -275),
              (-175, -225),

              (-175, -125),
              (-175, -75),
              (-175, -25),
              (-175, 25),
              
              (175, -375), 
              (175, -325), 
              (175, -275),
              (175, -225),
              
              (175, -125),
              (175, -75),
              (175, -25),
              (175, 25),
            
              (225, -275),
              (225, -75),

              (275, -375), 
              (275, -325), 
              (275, -275),
              (275, -225),

              (275, -125),
              (275, -75),
              (275, -25),
              (275, 25),
              
              (325, -275),
              (325, -75),

              (375, -375), 
              (375, -325), 
              (375, -275),
              (375, -225),

              (375, -125),
              (375, -75),
              (375, -25),
              (375, 25),
              -- criss crossies
              (-125, -375), (-125, -125),
              (-75, -75), (-75, -325),
              (-25, -325),
              (25, -25),
              (75, -275), (75, -25),
              (125, -225), (125, 25),
              -- big T
              (-375, 175),(-325, 175),(-275, 175),(-225, 175),(-175, 175),(-125, 175),(-75, 175),(-25, 175),
              (375, 175),(325, 175),(275, 175),(225, 175),(175, 175),(125, 175),(75, 175),(25, 175),
              (-375, 125),(375, 125),
              (-25, 125),(-25, 75),
              (25, 125),(25, 75)
              ]

lvl1SpecialCollectibles :: [Collectible] -- only the one for testing
lvl1SpecialCollectibles = [
  Collectible (GhostsOff 5.0) 5 red (275, -475),
  Collectible (GhostsOff 5.0) 5 red (-275, -475),
  Collectible (GhostsOff 5.0) 5 red (275, 225),
  Collectible (GhostsOff 5.0) 5 red (-275, 225),
  Collectible (GhostsOff 5.0) 5 red (275, -175),
  Collectible (GhostsOff 5.0) 5 red (-275, -175),
  Collectible (GhostsOff 5.0) 5 red (-75, 125),
  Collectible (GhostsOff 5.0) 5 red (75, 125)
  ]


{-
------------------------------------------------------------
UTILS

general utility funcs
------------------------------------------------------------
-}

outOfBounds :: Point -> Bool
outOfBounds (x, y) = x > 500 || x < -500 || y > 250 || y < -500

playerStartPoint :: Point
playerStartPoint = (-475, -475)

playerInitLives :: Int
playerInitLives = 3 -- todo: 3 for final

playerInitScore :: Int
playerInitScore = 0

blinkyStartPoint :: Point
blinkyStartPoint = (475, 225)

pinkyStartPoint :: Point
pinkyStartPoint = (475, -475)

inkyStartPoint :: Point
inkyStartPoint = (-475, 225)

clydeStartPoint :: Point
clydeStartPoint = (75, 25)

genLevel :: Int -> Board
genLevel lvlNum 
  | lvlNum == 1 = Board (genTiles lvl1Walls) (genPivots lvl1Walls) (genCollectibles (playerStartPoint:lvl1Walls) lvl1SpecialCollectibles) playerInitLives playerInitScore drawBoard updateBoard genPlayer genGhosts False []
  | otherwise = Board [] [] [] 0 0 drawBoard updateBoard genPlayer genGhosts False []




{-
------------------------------------------------------------
DECONSTRUCTORS

essentially getters for easy reading
------------------------------------------------------------
-}

-- deconDestination :: Neighbor -> Destination
-- deconDestination (Neighbor _ dest _) = dest

-- deconTracks :: Neighbor -> [Track]
-- deconTracks (Neighbor _ tracks) = tracks

{-
------------------------------------------------------------
DRAWING FUNCTIONS
------------------------------------------------------------
-}

-- todo: move drawplayer and draw ghosts here?
drawBoard :: Board -> [Picture]
drawBoard (Board ts ps cs l s d u (Player locP desP currP nextP dP uP coll) gs gOver timers) = drawScore s : drawLives l :  (drawGrid ts ++ drawCollectibles cs) ++ ((dP (Player locP desP currP nextP dP uP coll)) : drawGhosts gs)

drawGrid :: [Tile] -> [Picture] -- todo: change these to tail recursion
drawGrid  [] = [] 
drawGrid (t:ts) = drawTile t : drawBorder t : drawGrid ts

drawCollectibles :: [Collectible] -> [Picture]
drawCollectibles = map drawCollectible

drawCollectible :: Collectible -> Picture
drawCollectible (Collectible _ _ c (x, y)) = color c (translate x y (thickCircle 5 10))

drawLives :: Int -> Picture
drawLives l = scale 0.5 0.5 (translate (-1000) 800 (Text ("Lives: " ++ show l)))

drawScore :: Int -> Picture
drawScore s = scale 0.5 0.5 (translate (-1000) 600 (Text ("Score: " ++ show s)))

drawTile :: Tile -> Picture
drawTile (Tile c (Boundary bottom top left right)) = 
  color c (Polygon [(left, top), (right, top), (right, bottom), (left, bottom)])

drawBorder :: Tile -> Picture
drawBorder (Tile _ (Boundary bottom top left right)) = 
  color black (Line [(left, top), (right, top), (right, bottom), (left, bottom)])

drawPlayer :: Player -> Picture
drawPlayer (Player (x, y) _ _ _ _ _ _) = color yellow (translate x y (thickCircle 10 20))


drawGhosts :: [Ghost] -> [Picture]
drawGhosts gs = go [] gs--foldr go [] gs --go gs board []
  where 
    go acc [] = acc
    go acc ((Ghost name loc path curr next d u inf):gs) = go (d (Ghost name loc path curr next d u inf) : acc) gs 

drawGhost :: Color -> Ghost -> Picture
drawGhost c (Ghost _ (x, y) _ _ _ _ _ _) = color c (translate x y (thickCircle 10 20))



-- data Ghost = 
--   Ghost {
--     locationG :: Point,
--     pathG :: (Destination, [Track]),
--     currDirectionG :: Direction, 
--     nextDirectionG :: Direction,
--     drawG :: Ghost -> Picture,
--     updateG :: Ghost -> Board -> Ghost
--  }

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
UPDATE FUNCTIONS
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

{-
------------------------------------------------------------
DRAW FUNCTIONS
------------------------------------------------------------
-}






getPlayerLocation :: Board -> Point
getPlayerLocation (Board ts ps cs l s dB uB (Player loc _ _ _ _ _ _) gs gOver timers) = loc

getPlayerDestination :: Board -> Point
getPlayerDestination (Board ts ps cs l s dB uB (Player _ ( pt, _) _ _ _ _ _) gs gOver timers) = pt

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


-- allVisited :: Board -> Point -> [Point] -> Bool
-- allVisited b pt visited = null (filter (\(Neighbor)) next)  
--   where
--     next = getValidNeighbors b pt

getValidNeighbors :: Board -> Point -> [Neighbor]
getValidNeighbors b pt = filter (/= Null) [up,  right, left, down]
  where 
    (Pivot _ (up, down, left, right)) = getPiv (getPivot pt b)


-- dfsRefill :: Board -> Point -> Point -> Pivot -> [Point] -> [Track] -> [Track]
-- dfsRefill b dest curr (Pivot pt (up, down, left, right)) visited path
--   | dest == curr || null validNeighbors = path
--   | otherwise = 
--       dfsRefill b dest (getNeighborPoint (head validNeighbors)) (getPiv (getPivot (getNeighborPoint (head validNeighbors)) b)) (pt:visited) (path ++ (getNeighborTrack (head validNeighbors)))

    
getNeighborPoint :: Neighbor -> Point
getNeighborPoint (Neighbor _ dest _) = dest
getNeighborPoint _ = error "don't give this null, dummy"

-- getNeighborTrack :: Neighbor -> [Track]
-- getNeighborTrack (Neighbor _ track) = track
-- getNeighborTrack _ = error "don't give this null, dummy"

getPiv :: Maybe Pivot -> Pivot
getPiv (Just pv) = pv
getPiv _ = error "don't give nothing please"  



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

deconDest :: Neighbor -> Point
deconDest Null = error "what is wrong with you"
deconDest (Neighbor _ dest _) = dest 

