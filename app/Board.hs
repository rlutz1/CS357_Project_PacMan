{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
module Board where

import Brillo
import Data
import Utils
import Ghost
import Player
import BoardSetup

{-
  @author Roxanne Lutz
  the actual board specific functionality.
  all updating, drawing, and generation functions are within this file.
  
  some magic number nonsense in here, which i'm not proud of. 
  for the future, i would like to make the board dimension more
  flexible with the window size, but for this project this worked for
  the consistency to start.
-}

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

-- overarching collision detection function to see if pacman has collided with any of the ghosts.
checkCollision :: Board -> Player -> [Ghost] -> Board
checkCollision b p [] = b
checkCollision 
  (Board ts ps cs lives s dB uB op ogs gOver timers)
  (Player locP desP currP nextP dP uP coll) 
  ((Ghost name locG desG currG nextG dG uG inf):gs)
    | coll = 
      if collisionDetected locP locG 
        then kill (Board ts ps cs lives s dB uB op ogs gOver timers) 
        else checkCollision (Board ts ps cs lives s dB uB op ogs gOver timers) (Player locP desP currP nextP dP uP coll) gs
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
  | sameCol (x1, y1) (x2, y2) = max y1 y2 - min y1 y2 < collisionBumper
  | sameRow (x1, y1) (x2, y2) = max x1 x2 - min x1 x2 < collisionBumper
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

-- game was won, draw the board this way now, with a notification
drawGameWon :: Board -> [Picture]
drawGameWon b =  drawBoard b ++ drawGameWonNotification

-- game was lost, draw the board this way now, with a notification
drawGameLost :: Board -> [Picture]
drawGameLost b =  drawBoard b ++ drawGameLostNotification

-- the notification to put on screen for the player when they win the level
drawGameWonNotification :: [Picture]
drawGameWonNotification = [
  color (makeColor 0.616 0.616 0.616 1) (rectangleSolid 750 400),
  scale 0.25 0.25 (translate (-350) (200) (Text "YOU WON!")),
  scale 0.25 0.25 (translate (-1250) (0) (Text "You did escape the spooky ghosts!")),
  scale 0.25 0.25 (translate (-800) (-200) (Text "Press G to back to menu"))
  ]

-- the notification to put on screen for the player when they lose the level
drawGameLostNotification :: [Picture]
drawGameLostNotification = [
  color (makeColor 0.616 0.616 0.616 1) (rectangleSolid 750 400),
  scale 0.25 0.25 (translate (-400) (200) (Text "YOU FAILED!")),
  scale 0.25 0.25 (translate (-1250) (0) (Text "You did not escape the ghosts.")),
  scale 0.25 0.25 (translate (-800) (-200) (Text "Press G to back to menu"))
  ]

{-
------------------------------------------------------------
BOARD GENERATION FUNCTIONS
------------------------------------------------------------
-}

-- generate pivot points -> centers of the tiles of the board
genPivots :: [Point] -> [Pivot]
genPivots walls = 
  [Pivot p (genNeighbors UP p walls, genNeighbors DOWN p walls, genNeighbors LEFT p walls, genNeighbors RIGHT p walls) 
  | p <- genCenters, notElem p walls]

-- generate collectibles for the board, general ones only.
genCollectibles :: [Point] -> [Collectible] -> [Collectible]
genCollectibles walls specials = specials ++ [ Collectible NoEffect defaultCollScore orange p | p <- genCenters, notElem p walls, notElem p specialPoints]
  where
    specialPoints = [ pt | (Collectible _ _ _ pt) <- specials ]

-- generate all center points on the board. this math is done and specific to the window size. should be more flexible for future work.
genCenters :: [Point]
genCenters = [ (x, y) | x <- [-475, -425..475], y <- [-475, -425..225] ]

-- generate all neighbors from a specified pivot point
genNeighbors :: Direction -> Point -> [Point] -> Neighbor
genNeighbors UP (x, y) walls = genNeighbor UP (x, y) (x, y + 50) walls
genNeighbors DOWN (x, y) walls = genNeighbor DOWN (x, y) (x, y - 50) walls
genNeighbors LEFT (x, y) walls = genNeighbor LEFT (x, y) (x - 50, y)walls
genNeighbors RIGHT (x, y) walls = genNeighbor RIGHT (x, y) (x + 50, y) walls
genNeighbors _ _ _ = Null

-- a cleaner helper for the above
genNeighbor :: Direction -> Point -> Point -> [Point] -> Neighbor
genNeighbor dir piv pt walls
  | outOfBounds pt || elem pt walls = Null 
  | otherwise = Neighbor dir pt piv

-- generate the player on the board
genPlayer :: Player 
genPlayer = Player playerStartPoint ( playerStartPoint, []) NONE NONE drawPlayer updatePlayer False

-- generate the level depending on input from user
genLevel :: Int -> Board
genLevel 1 = Board (genTiles lvl1Walls) (genPivots lvl1Walls) (genCollectibles (playerStartPoint:lvl1Walls) lvl1SpecialCollectibles) playerInitLives playerInitScore drawBoard updateBoard genPlayer genGhosts False []
genLevel _ = Board [] [] [] 0 0 drawBoard updateBoard genPlayer genGhosts False []

-- generate the tiles for the board. tiles are mainly for graphical purposes only and can be both path or wall
genTiles :: [Point] -> [Tile]
genTiles walls = [ Tile (getTileColor walls (x + 25, y + 25)) (Boundary y (y + 50) x (x + 50)) | x <- [-500, -450..450], y <- [-500, -450..200] ]

-- return a color based on whether wall or not.
getTileColor :: [Point] -> Point -> Color
getTileColor walls center
  | elem center walls = black
  | otherwise = blue

-- simple helper to reference what is out of bounds at this specific window.
outOfBounds :: Point -> Bool
outOfBounds (x, y) = x > 500 || x < -500 || y > 250 || y < -500

{-
------------------------------------------------------------
BOARD DRAWING FUNCTIONS
------------------------------------------------------------
-}

-- draw the board pieces
drawBoard :: Board -> [Picture]
drawBoard (Board ts ps cs l s d u (Player locP desP currP nextP dP uP coll) gs gOver timers) = 
  drawScore s : drawLives l :  (drawGrid ts ++ drawCollectibles cs) ++ ((dP (Player locP desP currP nextP dP uP coll)) : drawGhosts gs)

-- draw the game board grid
drawGrid :: [Tile] -> [Picture]
drawGrid  [] = [] 
drawGrid (t:ts) = drawTile t : drawBorder t : drawGrid ts

-- draw all collectibles
drawCollectibles :: [Collectible] -> [Picture]
drawCollectibles = map drawCollectible

-- draw a single collectibles
drawCollectible :: Collectible -> Picture
drawCollectible (Collectible _ _ c (x, y)) = color c (translate x y (thickCircle collRadius collThickness))

-- draw how many lives left for pacman
drawLives :: Int -> Picture
drawLives l = scale 0.5 0.5 (translate (-1000) 800 (Text ("Lives: " ++ show l)))

-- draw the current pacman score
drawScore :: Int -> Picture
drawScore s = scale 0.5 0.5 (translate (-1000) 600 (Text ("Score: " ++ show s)))

-- draw the tiles of the board
drawTile :: Tile -> Picture
drawTile (Tile c (Boundary bottom top left right)) = 
  color c (Polygon [(left, top), (right, top), (right, bottom), (left, bottom)])

-- draw boarder so tiles/paths are more easily visible
drawBorder :: Tile -> Picture
drawBorder (Tile _ (Boundary bottom top left right)) = 
  color black (Line [(left, top), (right, top), (right, bottom), (left, bottom)])

{-
------------------------------------------------------------
BOARD CONSTANTS
------------------------------------------------------------
-}

collisionBumper :: Float
collisionBumper = 40

defaultCollScore :: Int
defaultCollScore = 10

collRadius :: Float
collRadius = 5

collThickness :: Float 
collThickness = 10
