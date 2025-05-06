{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
module Board where

import Brillo
import Data
import Data.Word
import System.Random
-- import Ghost
-- import Player

updateBoard :: Board -> Float -> Board
updateBoard (Board ts ps cs ls s dB uB p gs gOver timers) dt = 
  do
    let updatedP = updatePlayer p (Board ts ps cs ls s dB uB p gs gOver timers)
    let updatedGs = updateGhosts gs (Board ts ps cs ls s dB uB p gs gOver timers)
    let updatedColls = updateCollectibles (Board ts ps cs ls s dB uB updatedP updatedGs gOver timers)
    let updatedEffects = updateEffects updatedColls dt []
    checkCollision (updatedEffects) updatedP updatedGs

{-
for checking the timers, removing effects as necessary.
but we need to keep timers as they come

what is this method doing? 

it's basically there to see if the timer is up and revert
-}
updateEffects :: Board -> Float -> [(Effect, Float, Float)] -> Board
updateEffects (Board ts ps cs lives s dB uB op ogs gOver []) dt acc = (Board ts ps cs lives s dB uB op ogs gOver acc)
updateEffects (Board ts ps cs lives s dB uB op ogs gOver ((eff, timer, end):timers)) dt acc
  | timesUp (eff, timer, end) = updateEffects (removeEffect (Board ts ps cs lives s dB uB op ogs gOver timers) (eff, timer, end)) dt acc
  | otherwise = updateEffects (Board ts ps cs lives s dB uB op ogs gOver timers) dt ((eff, timer + dt , end):acc)--add time to timer and keep checking through

removeEffect :: Board -> (Effect, Float, Float) -> Board
removeEffect (Board ts ps cs lives s dB uB op ogs gOver timers) (NoEffect, _, _) = (Board ts ps cs lives s dB uB op ogs gOver timers) -- should never happen, but to be clean
removeEffect (Board ts ps cs lives s dB uB (Player locP desP currP nextP dP uP coll) ogs gOver timers) ((GhostsOff _), _, _)
  = Board ts ps cs lives s dB uB (Player locP desP currP nextP dP uP True) (drawGhostsOn ogs) gOver timers
-- turn on coll detection. -- update the drawing func i think

timesUp :: (Effect, Float, Float) -> Bool
timesUp (_, timer, end) = timer >= end

checkCollision :: Board -> Player -> [Ghost] -> Board
checkCollision b p [] = b
checkCollision 
  (Board ts ps cs lives s dB uB op ogs gOver timers)
  (Player locP desP currP nextP dP uP coll) 
  ((Ghost name locG desG currG nextG dG uG inf):gs)
    | coll = if collisionDetected locP locG then chugBeer (Board ts ps cs lives s dB uB op ogs gOver timers) else checkCollision (Board ts ps cs lives s dB uB op ogs gOver timers) (Player locP desP currP nextP dP uP coll) gs
    | otherwise = Board ts ps cs lives s dB uB op ogs gOver timers
 
chugBeer :: Board -> Board
chugBeer (Board ts ps cs lives s dB uB op ogs gOver timers) 
  | checkGameOver (lives - 1) = Board ts ps cs lives s drawGameOver (\b f -> b) op ogs True timers
  | otherwise = Board ts ps cs (lives - 1) s dB uB genPlayer genGhosts gOver timers

drawGameOver :: Board -> [Picture]
drawGameOver b =  drawBoard b ++ drawGameOverNotification

-- rgb(157, 157, 157)
drawGameOverNotification :: [Picture]
drawGameOverNotification = [
  color (makeColor 0.616 0.616 0.616 1) (rectangleSolid 750 400),
  scale 0.25 0.25 (translate (-400) (200) (Text "YOU FAILED!")),
  scale 0.25 0.25 (translate (-1250) (0) (Text "You did not escape the ghosts.")),
  scale 0.25 0.25 (translate (-800) (-200) (Text "Press G to back to menu"))
  ]

collisionDetected :: Point -> Point -> Bool
collisionDetected (x1, y1) (x2, y2)
  | sameCol (x1, y1) (x2, y2) = max y1 y2 - min y1 y2 < 40
  | sameRow (x1, y1) (x2, y2) = max x1 x2 - min x1 x2 < 40
  | otherwise = False
  
sameCol :: Point -> Point -> Bool
sameCol (x1, _) (x2, _) = x1 == x2

sameRow :: Point -> Point -> Bool
sameRow (_, y1) (_, y2) = y1 == y2
  
checkGameOver :: Int -> Bool
checkGameOver x = x < 0

{-
okay let's think it through.
when we find the collectible, we need to see what kind it is.
within the collectible there is an effect associated. 
it feels like we need to test upon the effect. 
what do we want to happen based on the cherry effect
-}
-- terribly inefficient, poorly organized, this would be better encapsed in player some how...but idk yet
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

enactEffect :: [Collectible] -> Maybe Collectible ->  Board -> Board

{-
to do here:
need to update the ghost drawing function to be like white or some shit, basic user feedback
trickiest part: need to have some sort of timing mechanism.

thots on timing.
could add another thing to the player or board.
i like player more.
it would be hypothetically reasonable to have many collectibles with diff effects
have a [] of what would effectively be timers
thinking something like (float, effect, float(end effect time?))
so that if we don't have that effect in the list, add it, start timer as 0, and set whatever the effect end time is (can be within the collectible)
if we already have that effect in the list, simply reset the timer. or add to end time, whatever makes sense
if timer >= end, effect is off, "remove" it from the timer list
  actually, have updateplayer check the effect timers as well as move the player. that's just fine.
  then we will need some sort of checkActiveEffects that takes the board that sees what effects are on going in the player's timers, and changes the drawing or update functions needed?

then it really just comes down to how to have a timer. 
we do ignore the time since last frame value in update in main rn. 
i could give that to the update function and see how that works.

-}

enactEffect -- maybe this could just take the player? if we're doing things elsewhere for the board
  filteredColls -- filtered out the eaten coll
  (Just (Collectible (GhostsOff time) score color' pos)) -- this should always be given here  
  (Board ts ps cs l s d u (Player loc dest curr next dp up collDetect) gs gOver timers) =
    Board ts ps filteredColls l (s + score) d u (Player loc dest curr next dp up False) (drawGhostsOff gs) gOver (addToTimers (GhostsOff time) timers) -- the board

enactEffect -- maybe this could just take the player? if we're doing things elsewhere for the board
  filteredColls -- filtered out the eaten coll
  (Just (Collectible _ score color' pos)) -- this should always be given here  
  (Board ts ps cs l s d u (Player loc dest curr next dp up collDetect) gs gOver timers) =
    Board ts ps filteredColls l (s + score) d u (Player loc dest curr next dp up collDetect) gs gOver timers

enactEffect _ Nothing b = b -- this hypothetically should never happen, but here anyway

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
-- test :: Player -> Picture
-- test (Player (x, y) _ _ _ _ _ _) = color red (translate x y (thickCircle 10 20))



addToTimers :: Effect -> [(Effect, Float, Float)] -> [(Effect, Float, Float)]
-- addToTimers _ ts = ts
addToTimers NoEffect ts = ts
addToTimers effect ts = go effect ts
  where 
    go eff [] = [(effect, 0, effectTime eff)]
    go eff ((e, runtimetime, end):ts) 
      | eff == e = (e, 0, end):ts
      | otherwise = (e, runtimetime, end) : go eff ts

effectTime :: Effect -> Float
effectTime (GhostsOff time) = time
effectTime NoEffect = 0

drawGameWon :: Board -> [Picture]
drawGameWon b =  drawBoard b ++ drawGameWonNotification

-- rgb(157, 157, 157)
drawGameWonNotification :: [Picture]
drawGameWonNotification = [
  color (makeColor 0.616 0.616 0.616 1) (rectangleSolid 750 400),
  scale 0.25 0.25 (translate (-350) (200) (Text "YOU WON!")),
  scale 0.25 0.25 (translate (-1250) (0) (Text "You did escape the spooky ghosts!")),
  scale 0.25 0.25 (translate (-800) (-200) (Text "Press G to back to menu"))
  ]

allEaten :: [Collectible] -> Bool
allEaten [] = True
allEaten _ = False

updateScore :: Maybe Collectible -> Int
updateScore Nothing = 0
updateScore (Just (Collectible _ s _ _)) = s

findColl :: Point -> [Collectible] -> Maybe Collectible
findColl p [] = Nothing
findColl p ((Collectible e s c pt):cs)
  | p == pt = Just (Collectible e s c pt)
  | otherwise = findColl p cs

deconCollLoc :: Collectible -> Point
deconCollLoc (Collectible _ _ _ p) = p

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
-- data Neighbor = Null | Neighbor Destination [Track]

genPivots :: [Point] -> [Pivot]
genPivots walls = [ Pivot p (getNeighbor UP p walls, getNeighbor DOWN p walls, getNeighbor LEFT p walls, getNeighbor RIGHT p walls) | p <- genCenters, notElem p walls]

genCollectibles :: [Point] -> [Collectible] -> [Collectible] -- for now, to get some basic ones "installed"
genCollectibles walls specials = specials ++ [ Collectible NoEffect 10 orange p | p <- genCenters, notElem p walls, notElem p specialPoints]
  where
    specialPoints = [ pt | (Collectible _ _ _ pt) <- specials ]

genCenters :: [Point]
genCenters = [ (x, y) | x <- [-475, -425..475], y <- [-475, -425..225] ]

getNeighbor :: Direction -> Point -> [Point] -> Neighbor
getNeighbor dir (x, y) walls 
  | dir == UP = if outOfBounds (x, y + 50) || elem (x, y + 50) walls then Null else Neighbor (Destination (x, y + 50)) (genTracks defaultSpeed UP (x, y) (x, y + 50))
  | dir == DOWN = if outOfBounds (x, y - 50) || elem (x, y - 50) walls then Null else Neighbor (Destination (x, y - 50)) (genTracks defaultSpeed DOWN (x, y) (x, y - 50))
  | dir == LEFT = if outOfBounds (x - 50, y) || elem (x - 50, y) walls then Null else Neighbor (Destination (x - 50, y)) (genTracks defaultSpeed LEFT (x, y) (x - 50, y))
  | dir == RIGHT = if outOfBounds (x + 50, y) || elem (x + 50, y) walls then Null else Neighbor (Destination (x + 50, y)) (genTracks defaultSpeed RIGHT (x, y) (x + 50, y))
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
genPlayer = Player playerStartPoint (Destination playerStartPoint, []) NONE NONE drawPlayer updatePlayer False

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

getTracks :: Maybe Pivot -> Direction -> Neighbor
getTracks Nothing _ = Null
getTracks (Just (Pivot _ (upN, downN, leftN, rightN))) dir
  | dir == UP = upN
  | dir == DOWN = downN
  | dir == LEFT = leftN
  | dir == RIGHT = rightN
  | otherwise = Null


{-
------------------------------------------------------------
DECONSTRUCTORS

essentially getters for easy reading
------------------------------------------------------------
-}

deconDestination :: Neighbor -> Destination
deconDestination (Neighbor dest _) = dest

deconTracks :: Neighbor -> [Track]
deconTracks (Neighbor _ tracks) = tracks

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
genBlinky = Ghost "Blinky" blinkyStartPoint (Destination blinkyStartPoint, []) NONE NONE (drawGhost blinkyDefColor) updateBlinky (uniforms (mkStdGen 42) :: [Int])

genPinky :: Ghost
genPinky = Ghost "Pinky" pinkyStartPoint (Destination pinkyStartPoint, []) NONE NONE (drawGhost pinkyDefColor) updatePinky (uniforms (mkStdGen 79) :: [Int])

genInky :: Ghost
genInky = Ghost "Inky" inkyStartPoint (Destination inkyStartPoint, []) NONE NONE (drawGhost inkyDefColor) updateInky (uniforms (mkStdGen 7) :: [Int])

genClyde :: Ghost
genClyde = Ghost "Clyde" clydeStartPoint (Destination clydeStartPoint, []) NONE NONE (drawGhost clydeDefColor) updateClyde (uniforms (mkStdGen 782) :: [Int])

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




{-
------------------------------------------------------------
GHOST MOVEMENT FUNCTIONS
------------------------------------------------------------
-}

-- i may not need directions for the ghosts
-- below are where the ai decision making movesments should live

-- the nuke
moveBlinky :: Ghost -> Board -> Ghost
moveBlinky (Ghost name loc (Destination point, []) curr next d u inf) b 
  = Ghost name loc (Destination point, [loc]) curr next d u inf
moveBlinky (Ghost name loc (Destination point, [t]) curr next d u inf) b 
  = Ghost name t (Destination  (getPlayerDestination b), nuke 1 b (getPlayerDestination b) (addPaths (getValidNeighbors b loc 1) []) [point]) curr next d u inf
moveBlinky (Ghost name loc (dest, t:ts) curr next d u inf) _ = Ghost name t (dest, ts) curr next d u inf

-- the nuke but only within line of sight, otherwise meanderer
movePinky :: Ghost -> Board -> Ghost
movePinky (Ghost name loc (Destination point, []) curr next d u inf) b 
  = Ghost name loc (Destination point, [loc]) curr next d u inf

movePinky (Ghost name (x, y) (Destination point, [t]) curr next d u inf) b
  | sameCol (getPlayerLocation b) (x, y) || sameRow (getPlayerLocation b) (x, y) = Ghost name t (Destination (getPlayerDestination b), nuke (getRandomOrder x) b (getPlayerDestination b) (addPaths (getValidNeighbors b (x, y) (getRandomOrder x)) []) [point]) curr next d u inf-- NUKE
  | otherwise = Ghost name t (Destination  (getPlayerDestination b), meander (head inf) b (giveRandomNeighbor (getValidNeighbors b (x, y) (head inf)) (head inf)) [point] []) curr next d u (tail inf)
  -- = Ghost name t (Destination  (getPlayerDestination b), meander (getRandomOrder x) b (head (getValidNeighbors b (x, y) (getRandomOrder x))) [point] []) curr next d u 

movePinky (Ghost name loc (Destination point, t:ts) curr next d u inf) b = Ghost name t (Destination point, ts) curr next d u inf
  -- | withinLineOfSight (getPlayerLocation b) loc = Ghost name loc (Destination (getPlayerDestination b), nuke 1 b (getPlayerDestination b) (addPaths (getValidNeighbors b loc 1) []) [point]) curr next d u -- NUKE
  -- | otherwise = Ghost name t (Destination point, ts) curr next d u -- just continue on

withinLineOfSight :: Point -> Point -> Bool
withinLineOfSight p g = sameCol p g || sameRow p g



getRandomOrder :: Float -> Int -- REMOVE
getRandomOrder x = (abs (round x) * 7) `mod` 2
-- getRandomOrder :: Float -> Int
-- getRandomOrder _ = 2

-- a meanderer
moveInky :: Ghost -> Board -> Ghost
moveInky (Ghost name loc (Destination point, []) curr next d u inf) b 
  = Ghost name loc (Destination point, [loc]) curr next d u inf
moveInky (Ghost name (x, y) (Destination point, [t]) curr next d u inf) b 
  = Ghost name t (Destination  (getPlayerDestination b), meander (head inf) b (giveRandomNeighbor (getValidNeighbors b (x, y) (head inf)) (head inf)) [point] []) curr next d u (tail inf)
moveInky (Ghost name loc (dest, t:ts) curr next d u inf) _ = Ghost name t (dest, ts) curr next d u inf

-- a meanderer
moveClyde :: Ghost -> Board -> Ghost
moveClyde (Ghost name loc (Destination point, []) curr next d u inf) b 
  = Ghost name loc (Destination point, [loc]) curr next d u inf
moveClyde (Ghost name (x, y) (Destination point, [t]) curr next d u inf) b 
  = Ghost name t (Destination  (getPlayerDestination b), meander (head inf) b (giveRandomNeighbor (getValidNeighbors b (x, y) (head inf)) (head inf)) [point] []) curr next d u (tail inf)
moveClyde (Ghost name loc (dest, t:ts) curr next d u inf) _ = Ghost name t (dest, ts) curr next d u inf



giveRandomNeighbor :: [Neighbor] -> Int -> Neighbor
giveRandomNeighbor [] _ = Null
giveRandomNeighbor ns seed = head shuffled
  where
    shuffled = fst (uniformShuffleList ns (mkStdGen seed))

getPlayerLocation :: Board -> Point
getPlayerLocation (Board ts ps cs l s dB uB (Player loc _ _ _ _ _ _) gs gOver timers) = loc

getPlayerDestination :: Board -> Point
getPlayerDestination (Board ts ps cs l s dB uB (Player _ (Destination pt, _) _ _ _ _ _) gs gOver timers) = pt


meander :: Int -> Board -> Neighbor -> [Point] -> [Track] -> [Track]
meander seed b  (Neighbor (Destination pt) trackToNeighbor) visited path 
  | null unvisited = path
  | otherwise =  meander seed b (giveRandomNeighbor unvisited seed) (pt:visited) (path ++ trackToNeighbor)
  where 
    next = getValidNeighbors b pt seed
    unvisited = getUnvisited next visited

getUnvisited :: [Neighbor] -> [Point] -> [Neighbor]
getUnvisited [] _ = []
getUnvisited ((Neighbor (Destination pt) track):ns) vis 
  | pt `elem` vis = getUnvisited ns vis
  | otherwise = (Neighbor (Destination pt) track) : getUnvisited ns vis

-- (genTracks defaultSpeed UP (x, y) (x, y + 50)

nukeButSlow :: Int -> Board -> Point -> [(Neighbor, [Track])] ->  [Point] -> [Track]
nukeButSlow order b dest ((Neighbor (Destination pt) tracksToNeighbor, path):queue) visited 
  | pt == dest = path ++ (genTracks (2.0 / 3.0) (getDir pt (head tracksToNeighbor)) (head tracksToNeighbor) pt)
  | otherwise = 
    if pt `elem` visited 
      then nukeButSlow order b dest (queue) (visited) 
      else  nukeButSlow order b dest (queue ++ nextsWithPaths) (pt:visited)
      -- if not (null recur) then prev ++ recur else recur
  -- | otherwise = tracksToNeighbor ++ recur
  where
    next = getValidNeighbors b pt order
    nextsWithPaths = addPaths next (path ++ (genTracks (2.0 / 3.0) (getDir pt (head tracksToNeighbor)) (head tracksToNeighbor) pt))
    getDir (x1, y1) (x2, y2) 
      | sameCol (x1, y1) (x2, y2) && y1 > y2 = UP
      | sameCol (x1, y1) (x2, y2) && y1 < y2 = DOWN
      | sameRow (x1, y1) (x2, y2) && x1 > x2 = RIGHT
      | otherwise = LEFT

    -- recur = nuke order b dest (queue ++ next) (pt:visited) tracksToNeighbor

nuke :: Int -> Board -> Point -> [(Neighbor, [Track])] ->  [Point] -> [Track]
nuke order b dest ((Neighbor (Destination pt) tracksToNeighbor, path):queue) visited 
  | pt == dest = path ++ tracksToNeighbor
  | otherwise = 
    if pt `elem` visited 
      then nuke order b dest (queue) (visited) 
      else  nuke order b dest (queue ++ nextsWithPaths) (pt:visited)
      -- if not (null recur) then prev ++ recur else recur
  -- | otherwise = tracksToNeighbor ++ recur
  where
    next = getValidNeighbors b pt order
    nextsWithPaths = addPaths next (path ++ tracksToNeighbor)
    -- recur = nuke order b dest (queue ++ next) (pt:visited) tracksToNeighbor

addPaths :: [Neighbor] -> [Track] -> [(Neighbor, [Track])]
addPaths [] path = []
addPaths (n:ns) path = (n, path) : addPaths ns path

notAdjacent :: Point -> Point -> Bool
notAdjacent (x1, y1) (x2, y2)
  | sameCol (x1, y1) (x2, y2) = max y1 y2 - min y1 y2 > 50
  | sameRow (x1, y1) (x2, y2) = max x1 x2 - min x1 x2 > 50
  | otherwise = True

-- allVisited :: Board -> Point -> [Point] -> Bool
-- allVisited b pt visited = null (filter (\(Neighbor)) next)  
--   where
--     next = getValidNeighbors b pt

getValidNeighbors :: Board -> Point -> Int -> [Neighbor]
getValidNeighbors b pt arg
  | arg == 1  = filter (/= Null) [up,  right, left, down]  
  | arg == 2 = filter (/= Null) [left, down, right, up]  
  | otherwise = filter (/= Null) [down, right, up,  left]  
  where 
    (Pivot _ (up, down, left, right)) = getPiv (getPivot pt b)


-- dfsRefill :: Board -> Point -> Point -> Pivot -> [Point] -> [Track] -> [Track]
-- dfsRefill b dest curr (Pivot pt (up, down, left, right)) visited path
--   | dest == curr || null validNeighbors = path
--   | otherwise = 
--       dfsRefill b dest (getNeighborPoint (head validNeighbors)) (getPiv (getPivot (getNeighborPoint (head validNeighbors)) b)) (pt:visited) (path ++ (getNeighborTrack (head validNeighbors)))

    
getNeighborPoint :: Neighbor -> Point
getNeighborPoint (Neighbor (Destination pt) _) = pt
getNeighborPoint _ = error "don't give this null, dummy"

getNeighborTrack :: Neighbor -> [Track]
getNeighborTrack (Neighbor _ track) = track
getNeighborTrack _ = error "don't give this null, dummy"

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
  | curr /= next && closeEnough ts = changeDir (Player t (dest, ts) curr next d u coll) b
  | otherwise = sameDir (Player t (dest, ts) curr curr d u coll) b
    
-- queueTracks :: Player -> Board -> Player
-- queueTracks (Player loc (dest, ts) curr next) b -- we are attempting oto queue up the next move
--   | curr /= next = changeDir (Player loc (dest, ts) curr next) b 
--   | otherwise = sameDir (Player loc (dest, ts) curr next) b 

-- todo clean up
changeDir :: Player -> Board -> Player
changeDir (Player loc (Destination point, ts) curr next d u coll) b 
  | nextTracks == Null = sameDir (Player loc (Destination point, ts) curr curr d u coll) b 
  | otherwise = Player loc (deconDestination nextTracks, ts ++ deconTracks nextTracks) next next d u coll
  where 
    nextPiv = getPivot point b
    nextTracks = getTracks nextPiv next



-- todo clean up
sameDir :: Player -> Board -> Player
sameDir (Player loc (Destination point, []) curr _ d u coll) _ = Player loc (Destination point, []) curr curr d u coll
sameDir (Player loc (Destination point, [t]) curr _ d u coll) b
  | nextTracks == Null = Player loc (Destination point, [t]) curr curr d u coll
  | otherwise = Player loc (deconDestination nextTracks, t :deconTracks nextTracks) curr curr d u coll
  where 
    nextPiv = getPivot point b
    nextTracks = getTracks nextPiv curr
sameDir (Player loc (Destination point, ts) curr next d u coll) _ = Player loc (Destination point, ts) curr next d u coll 

closeEnough :: [a] -> Bool
closeEnough xs = length xs <= 25




