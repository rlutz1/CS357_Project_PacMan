module Board where

import Brillo
import Data
-- import Ghost
-- import Player

updateBoard :: Board -> Board
updateBoard (Board ts ps cs ls s dB uB p gs gOver timers) = 
  do
    let updatedP = updatePlayer p (Board ts ps cs ls s dB uB p gs gOver timers)
    let updatedGs = updateGhosts gs (Board ts ps cs ls s dB uB p gs gOver timers)
    let updatedColls = updateCollectibles (Board ts ps cs ls s dB uB updatedP updatedGs gOver timers)
    let updatedEffects = updateEffects updatedColls
    checkCollision (updatedEffects) updatedP updatedGs

updateEffects :: Board -> Board
updateEffects b = b

checkCollision :: Board -> Player -> [Ghost] -> Board
checkCollision b p [] = b
checkCollision 
  (Board ts ps cs lives s dB uB op ogs gOver timers)
  (Player locP desP currP nextP dP uP coll) 
  ((Ghost locG desG currG nextG dG uG):gs)
    | coll = if collisionDetected locP locG then chugBeer (Board ts ps cs lives s dB uB op ogs gOver timers) else checkCollision (Board ts ps cs lives s dB uB op ogs gOver timers) (Player locP desP currP nextP dP uP coll) gs
    | otherwise = Board ts ps cs lives s dB uB op ogs gOver timers
 
chugBeer :: Board -> Board
chugBeer (Board ts ps cs lives s dB uB op ogs gOver timers) 
  | checkGameOver (lives - 1) = Board ts ps cs lives s drawGameOver id op ogs True timers
  | otherwise = Board ts ps cs (lives - 1) s dB uB genPlayer genGhosts gOver timers

drawGameOver :: Board -> [Picture]
drawGameOver b =  drawBoard b ++ drawGameOverNotification

-- rgb(157, 157, 157)
drawGameOverNotification :: [Picture]
drawGameOverNotification = [
  color (makeColor 0.616 0.616 0.616 1) (rectangleSolid 750 400),
  scale 0.25 0.25 (translate (-400) (200) (Text "YOU FAILED!")),
  scale 0.25 0.25 (translate (-1250) (0) (Text "You did not escape the boys in the alley.")),
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
      then Board ts ps cs l s drawGameWon id (Player loc dest curr next dp up collDetect) gs True timers
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
    Board ts ps filteredColls l (s + score) d u (Player loc dest curr next dp up False) gs gOver (addToTimers (GhostsOff time) timers) -- the board

enactEffect -- maybe this could just take the player? if we're doing things elsewhere for the board
  filteredColls -- filtered out the eaten coll
  (Just (Collectible _ score color' pos)) -- this should always be given here  
  (Board ts ps cs l s d u (Player loc dest curr next dp up collDetect) gs gOver timers) =
    Board ts ps filteredColls l (s + score) d u (Player loc dest curr next dp up collDetect) gs gOver timers

enactEffect _ Nothing b = b -- this hypothetically should never happen, but here anyway

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
  scale 0.25 0.25 (translate (-1250) (0) (Text "You did not escaped the boys in the alley.")),
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

genCollectibles :: [Point] -> [(Point, Collectible)] -> [Collectible] -- for now, to get some basic ones "installed"
genCollectibles walls specials = specialColls ++ [ Collectible NoEffect 10 orange p | p <- genCenters, notElem p walls, notElem p specialPoints]
  where
    specialPoints = [ p | (p, _) <- specials ]
    specialColls = [ c | (_, c) <- specials ]

genCenters :: [Point]
genCenters = [ (x, y) | x <- [-475, -425..475], y <- [-475, -425..225] ]

getNeighbor :: Direction -> Point -> [Point] -> Neighbor
getNeighbor dir (x, y) walls 
  | dir == UP = if outOfBounds (x, y + 50) || elem (x, y + 50) walls then Null else Neighbor (Destination (x, y + 50)) (genTracks UP (x, y) (x, y + 50))
  | dir == DOWN = if outOfBounds (x, y - 50) || elem (x, y - 50) walls then Null else Neighbor (Destination (x, y - 50)) (genTracks DOWN (x, y) (x, y - 50))
  | dir == LEFT = if outOfBounds (x - 50, y) || elem (x - 50, y) walls then Null else Neighbor (Destination (x - 50, y)) (genTracks LEFT (x, y) (x - 50, y))
  | dir == RIGHT = if outOfBounds (x + 50, y) || elem (x + 50, y) walls then Null else Neighbor (Destination (x + 50, y)) (genTracks RIGHT (x, y) (x + 50, y))
  | otherwise = Null

genTracks :: Direction -> Point -> Point -> [Track]
genTracks dir start end
  | dir == UP = go (>=) start end [] 0 1
  | dir == DOWN = go (<=) start end [] 0 (-1)
  | dir == LEFT = go (<=) start end [] (-1) 0
  | dir == RIGHT = go (>=) start end [] 1 0
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
genPlayer = Player playerStartPoint (Destination playerStartPoint, []) NONE NONE drawPlayer updatePlayer True

{-
------------------------------------------------------------
WALLS

specify walls by the center point only.
------------------------------------------------------------
-}

lvl1Walls :: [Point]
lvl1Walls =  [
  (-375, -375), 
  (-375, -325), 
  (-375, -275),
  (-375, -225),

  (-375, -125),
  (-375, -75),
  (-375, -25)
  ]

lvl1SpecialCollectibles :: [(Point, Collectible)] -- only the one for testing
lvl1SpecialCollectibles = [((-375, -475), Collectible (GhostsOff 5.0) 5 red (-375, -475))]


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

hankStartPoint :: Point
hankStartPoint = (475, 225)

daleStartPoint :: Point
daleStartPoint = (475, -475)

boomhauerStartPoint :: Point
boomhauerStartPoint = (-475, 225)

billStartPoint :: Point
billStartPoint = (75, 25)

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
drawBoard (Board ts ps cs l s d u p gs gOver timers) = drawScore s : drawLives l :  (drawGrid ts ++ drawCollectibles cs) ++ (drawPlayer p : drawGhosts gs)

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
    go acc ((Ghost loc path curr next d u):gs) = go (d (Ghost loc path curr next d u) : acc) gs 

-- note: get colord for make color by x / 255. friggin clamped [0, 1] not [0, 255] lol
-- rgb(137, 61, 2)
drawHank :: Ghost -> Picture
drawHank (Ghost (x, y) _ _ _ _ _) = color c (translate x y (thickCircle 10 20))
  where 
    c = makeColor 0.537 0.239 0.008 1

-- rgb(211, 40, 10) 
drawDale :: Ghost -> Picture
drawDale (Ghost (x, y) _ _ _ _ _) = color c (translate x y (thickCircle 10 20))
  where 
    c = makeColor 0.827 0.157 0.039 1
--rgba(255, 255, 255, 0)
-- rgb(225, 230, 144)
drawBoomhauer :: Ghost -> Picture
drawBoomhauer (Ghost (x, y) _ _ _ _ _) = color c (translate x y (thickCircle 10 20))
  where 
    c = makeColor 0.882 0.901 0.565 1

-- rgb(206, 167, 120) 
drawBill :: Ghost -> Picture
drawBill (Ghost (x, y) _ _ _ _ _) = color c (translate x y (thickCircle 10 20))
  where
    c = makeColor 0.808 0.655 0.471 1 









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
genGhosts = [genHank, genDale, genBoomhauer, genBill]

genHank :: Ghost
genHank = Ghost hankStartPoint (Destination hankStartPoint, []) NONE NONE drawHank updateHank

genDale :: Ghost
genDale = Ghost daleStartPoint (Destination daleStartPoint, []) NONE NONE drawDale updateDale

genBoomhauer :: Ghost
genBoomhauer = Ghost boomhauerStartPoint (Destination boomhauerStartPoint, []) NONE NONE drawBoomhauer updateBoomhauer

genBill :: Ghost
genBill = Ghost billStartPoint (Destination billStartPoint, []) NONE NONE drawBill updateBill




{-
------------------------------------------------------------
UPDATE FUNCTIONS
------------------------------------------------------------
-}

updateGhosts :: [Ghost] -> Board -> [Ghost]
updateGhosts gs board = go [] gs board--foldr go [] gs --go gs board []
  where 
    go acc [] _ = acc
    go acc ((Ghost loc path curr next d u):gs) board = go (u (Ghost loc path curr next d u) board : acc) gs board

updateHank :: Ghost -> Board -> Ghost
updateHank g board = moveHank g board

updateDale :: Ghost -> Board -> Ghost
updateDale g board = moveDale g board

updateBoomhauer :: Ghost -> Board -> Ghost
updateBoomhauer g board = moveBoomhauer g board

updateBill :: Ghost -> Board -> Ghost
updateBill g board = moveBill g board

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

moveHank :: Ghost -> Board -> Ghost
moveHank (Ghost loc (Destination point, []) curr next d u) b  = Ghost loc (Destination point, [loc]) curr next d u
moveHank (Ghost loc (Destination point, [t]) curr next d u) b = 
  Ghost t (Destination  (getPlayerDestination b), bfsRefill 1 b (getPlayerDestination b) (addPaths (getValidNeighbors b loc 1) []) [point]) curr next d u
moveHank (Ghost loc (dest, t:ts) curr next d u) _ = Ghost t (dest, ts) curr next d u


moveDale :: Ghost -> Board -> Ghost
moveDale (Ghost loc (Destination point, []) curr next d u) b  = Ghost loc (Destination point, [loc]) curr next d u
moveDale (Ghost (x, y) (Destination point, [t]) curr next d u) b = 
  Ghost t (Destination  (getPlayerDestination b), dfsRefill (getRandomOrder x) b (getPlayerDestination b) (getValidNeighbors b (x, y) (getRandomOrder x)) [point] []) curr next d u
moveDale (Ghost (x, y) (dest, t:ts) curr next d u) _ = Ghost t (dest, ts) curr next d u

getRandomOrder :: Float -> Int
getRandomOrder x = (abs (round x) * 7) `mod` 2
-- getRandomOrder :: Float -> Int
-- getRandomOrder _ = 2

moveBoomhauer :: Ghost -> Board -> Ghost
moveBoomhauer (Ghost loc (Destination point, []) curr next d u) b  = Ghost loc (Destination point, [loc]) curr next d u
moveBoomhauer (Ghost (x, y) (Destination point, [t]) curr next d u) b = 
  Ghost t (Destination  (getPlayerDestination b), dfsRefillV2 (getRandomOrder y) b (getPlayerDestination b) (head (getValidNeighbors b (x, y) (getRandomOrder y))) [point]) curr next d u
moveBoomhauer (Ghost loc (dest, t:ts) curr next d u) _ = Ghost t (dest, ts) curr next d u
-- moveBoomhauer (Ghost loc (Destination point, []) curr next d u) b  = Ghost loc (Destination point, [loc]) curr next d u
-- moveBoomhauer (Ghost loc (Destination point, [t]) curr next d u) b = Ghost t (deconDestination (head validDirs), deconTracks (head validDirs)) curr next d u
--   where 
--     nextPiv = getPivot point b
--     up = getTracks nextPiv UP
--     down = getTracks nextPiv DOWN
--     left = getTracks nextPiv LEFT
--     right = getTracks nextPiv RIGHT
--     validDirs = dumbShuffle (filter (/= Null) [up,  right, down, left ])
-- moveBoomhauer (Ghost loc (dest, t:ts) curr next d u) _ = Ghost t (dest, ts) curr next d u

moveBill :: Ghost -> Board -> Ghost
moveBill (Ghost loc (Destination point, []) curr next d u) b  = Ghost loc (Destination point, [loc]) curr next d u
moveBill (Ghost (x, y) (Destination point, [t]) curr next d u) b = 
  Ghost t (Destination  (getPlayerDestination b), dfsRefill (getRandomOrder x) b (getPlayerDestination b) (getValidNeighbors b (x, y) (getRandomOrder x)) [point] []) curr next d u
moveBill (Ghost loc (dest, t:ts) curr next d u) _ = Ghost t (dest, ts) curr next d u
-- moveBill (Ghost loc (Destination point, []) curr next d u) b  = Ghost loc (Destination point, [loc]) curr next d u
-- moveBill (Ghost loc (Destination point, (t:ts)) curr next d u) b
--   | length ts < 50 =
--     Ghost t (Destination  (getPlayerDestination b), ts ++ dfsRefill b (getPlayerDestination b) (getValidNeighbors b point) [] []) curr next d u
--   | otherwise = Ghost t (Destination point, ts) curr next d u

getPlayerLocation :: Board -> Point
getPlayerLocation (Board ts ps cs l s dB uB (Player loc _ _ _ _ _ _) gs gOver timers) = loc

getPlayerDestination :: Board -> Point
getPlayerDestination (Board ts ps cs l s dB uB (Player _ (Destination pt, _) _ _ _ _ _) gs gOver timers) = pt

dfsRefillV2 :: Int -> Board -> Point -> Neighbor ->  [Point] -> [Track]
dfsRefillV2 order b dest (Neighbor (Destination pt) trackToNeighbor) visited
  | pt == dest = trackToNeighbor
  | pt `elem` visited = []
  | otherwise = forEachNeighbor next
  where 
    next = getValidNeighbors b pt order -- why does 3 and 4 black screen of death the damn thing?????
    forEachNeighbor [] = []
    forEachNeighbor (n:neighbors) 
      | null (dfsRefillV2 order b dest n (pt:visited)) = forEachNeighbor neighbors
      | otherwise = trackToNeighbor ++ dfsRefillV2 order b dest n (pt:visited)


-- board, destination, current point, visited list -> how we got here -> path list -> RETURN the path to take
dfsRefill :: Int -> Board -> Point -> [Neighbor] ->  [Point] -> [Track] -> [Track]
dfsRefill order b dest ((Neighbor (Destination pt) trackToNeighbor):stack) visited path 
  | not (null path) && notAdjacent pt (last path) = path
  -- | null next = path
  | pt == dest = path ++ trackToNeighbor -- pathToGetHere ++ trackToNeighbor
  | otherwise = 
    if pt `elem` visited 
      then dfsRefill order b dest (stack) (visited) path
      else  dfsRefill order b dest (next ++ stack) (pt:visited) (path ++ trackToNeighbor)
  where 
    next = getValidNeighbors b pt order

bfsRefill :: Int -> Board -> Point -> [(Neighbor, [Track])] ->  [Point] -> [Track]
bfsRefill order b dest ((Neighbor (Destination pt) tracksToNeighbor, path):queue) visited 
  | pt == dest = path ++ tracksToNeighbor
  | otherwise = 
    if pt `elem` visited 
      then bfsRefill order b dest (queue) (visited) 
      else  bfsRefill order b dest (queue ++ nextsWithPaths) (pt:visited)
      -- if not (null recur) then prev ++ recur else recur
  -- | otherwise = tracksToNeighbor ++ recur
  where
    next = getValidNeighbors b pt order
    nextsWithPaths = addPaths next (path ++ tracksToNeighbor)
    -- recur = bfsRefill order b dest (queue ++ next) (pt:visited) tracksToNeighbor

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
  | arg == 1  = filter (/= Null) [up,  left, down, right]  
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

dumbShuffle :: [a] -> [a]
dumbShuffle xs = tail r ++ [head r]
  where 
    r = reverse xs




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




