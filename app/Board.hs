module Board where

import Brillo
import Data
-- import Ghost
-- import Player

updateBoard :: Board -> Board
updateBoard (Board ts ps cs ls s dB uB p gs gOver) = 
  do
    let updatedP = updatePlayer p (Board ts ps cs ls s dB uB p gs gOver)
    let updatedGs = updateGhosts gs (Board ts ps cs ls s dB uB p gs gOver)
    checkCollision (updateCollectibles (Board ts ps cs ls s dB uB updatedP updatedGs gOver)) updatedP updatedGs



checkCollision :: Board -> Player -> [Ghost] -> Board
checkCollision b p [] = b
checkCollision 
  (Board ts ps cs lives s dB uB op ogs gOver)
  (Player locP desP currP nextP dP uP coll) 
  ((Ghost locG desG currG nextG dG uG):gs)
    | coll = if collisionDetected locP locG then chugBeer (Board ts ps cs lives s dB uB op ogs gOver) else checkCollision (Board ts ps cs lives s dB uB op ogs gOver) (Player locP desP currP nextP dP uP coll) gs
    | otherwise = Board ts ps cs lives s dB uB op ogs gOver
 
chugBeer :: Board -> Board
chugBeer (Board ts ps cs lives s dB uB op ogs gOver) 
  | checkGameOver (lives - 1) = Board ts ps cs lives s drawGameOver id op ogs True
  | otherwise = Board ts ps cs (lives - 1) s dB uB genPlayer genGhosts gOver

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


-- terribly inefficient, poorly organized, this would be better encapsed in player some how...but idk yet
updateCollectibles :: Board -> Board 
updateCollectibles (Board ts ps cs l s d u (Player loc dest curr next dp up coll) gs gOver) 
  | length filteredColls < length cs = Board ts ps (Eaten loc : filteredColls) l (s + updateScore (findColl loc cs)) d u (Player loc dest curr next dp up coll) gs gOver
  | otherwise = if allEaten cs then Board ts ps cs l s drawGameWon id (Player loc dest curr next dp up coll) gs True else Board ts ps cs l s d u (Player loc dest curr next dp up coll) gs gOver
  where
    filteredColls = filter (\c -> deconCollLoc c /= loc) cs

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
allEaten (Eaten _:cs) = allEaten cs  
allEaten (Collectible _ _ _ _:cs) = False

updateScore :: Maybe Collectible -> Int
updateScore Nothing = 0
updateScore (Just (Collectible _ s _ _)) = s
updateScore (Just (Eaten _)) = 0

findColl :: Point -> [Collectible] -> Maybe Collectible
findColl p [] = Nothing
findColl p (Eaten _:cs) = findColl p cs
findColl p ((Collectible e s c pt):cs)
  | p == pt = Just (Collectible e s c pt)
  | otherwise = findColl p cs

deconCollLoc :: Collectible -> Point
deconCollLoc (Eaten p) = p
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

genCollectibles :: [Point] -> [Collectible] -- for now, to get some basic ones "installed"
genCollectibles walls = [ Collectible NoEffect 10 orange p | p <- genCenters, notElem p walls]

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
getPivot _ (Board _ [] _ _ _ _ _ _ _ _) = Nothing
getPivot point (Board ts ((Pivot pt ns):ps) cs l s d u p gs gOver)
  | point == pt = Just (Pivot pt ns)
  | otherwise = getPivot point (Board ts ps cs l s d u p gs gOver) 

genPlayer :: Player 
genPlayer = Player playerStartPoint (Destination playerStartPoint, []) NONE NONE drawPlayer updatePlayer True

{-
------------------------------------------------------------
WALLS

specify walls by the center point only.
------------------------------------------------------------
-}

lvl1Walls :: [Point]
lvl1Walls =  [(-375, -375), (175, 175), (25, 25)]

lvl1SpecialCollectibles :: [(Collectible, Point)]
lvl1SpecialCollectibles = undefined


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
daleStartPoint = (425, 225)

boomhauerStartPoint :: Point
boomhauerStartPoint = (375, 225)

billStartPoint :: Point
billStartPoint = (325, 225)

genLevel :: Int -> Board
genLevel lvlNum 
  | lvlNum == 1 = Board (genTiles lvl1Walls) (genPivots lvl1Walls) (genCollectibles (playerStartPoint:lvl1Walls)) playerInitLives playerInitScore drawBoard updateBoard genPlayer genGhosts False
  | otherwise = Board [] [] [] 0 0 drawBoard updateBoard genPlayer genGhosts False

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
drawBoard (Board ts ps cs l s d u p gs gOver) = drawScore s : drawLives l :  (drawGrid ts ++ drawCollectibles cs) ++ (drawPlayer p : drawGhosts gs)

drawGrid :: [Tile] -> [Picture] -- todo: change these to tail recursion
drawGrid  [] = [] 
drawGrid (t:ts) = drawTile t : drawBorder t : drawGrid ts

drawCollectibles :: [Collectible] -> [Picture]
drawCollectibles = map drawCollectible

drawCollectible :: Collectible -> Picture
drawCollectible (Eaten _) = Blank
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
moveHank (Ghost loc (Destination point, [t]) curr next d u) b = Ghost t (deconDestination (head validDirs), deconTracks (head validDirs)) curr next d u
  where 
    nextPiv = getPivot point b
    up = getTracks nextPiv UP
    down = getTracks nextPiv DOWN
    left = getTracks nextPiv LEFT
    right = getTracks nextPiv RIGHT
    validDirs = dumbShuffle (filter (/= Null) [up,  left, down, right ])
moveHank (Ghost loc (dest, t:ts) curr next d u) _ = Ghost t (dest, ts) curr next d u


moveDale :: Ghost -> Board -> Ghost
moveDale (Ghost loc (Destination point, []) curr next d u) b  = Ghost loc (Destination point, [loc]) curr next d u
moveDale (Ghost loc (Destination point, [t]) curr next d u) b = Ghost t (deconDestination (head validDirs), deconTracks (head validDirs)) curr next d u
  where 
    nextPiv = getPivot point b
    up = getTracks nextPiv UP
    down = getTracks nextPiv DOWN
    left = getTracks nextPiv LEFT
    right = getTracks nextPiv RIGHT
    validDirs = dumbShuffle (filter (/= Null) [down,  left, up, right ])
moveDale (Ghost loc (dest, t:ts) curr next d u) _ = Ghost t (dest, ts) curr next d u

moveBoomhauer :: Ghost -> Board -> Ghost
moveBoomhauer (Ghost loc (Destination point, []) curr next d u) b  = Ghost loc (Destination point, [loc]) curr next d u
moveBoomhauer (Ghost loc (Destination point, [t]) curr next d u) b = Ghost t (deconDestination (head validDirs), deconTracks (head validDirs)) curr next d u
  where 
    nextPiv = getPivot point b
    up = getTracks nextPiv UP
    down = getTracks nextPiv DOWN
    left = getTracks nextPiv LEFT
    right = getTracks nextPiv RIGHT
    validDirs = dumbShuffle (filter (/= Null) [up,  right, down, left ])
moveBoomhauer (Ghost loc (dest, t:ts) curr next d u) _ = Ghost t (dest, ts) curr next d u

moveBill :: Ghost -> Board -> Ghost
moveBill (Ghost loc (Destination point, []) curr next d u) b  = Ghost loc (Destination point, [loc]) curr next d u
moveBill (Ghost loc (Destination point, [t]) curr next d u) b = Ghost t (deconDestination (head validDirs), deconTracks (head validDirs)) curr next d u
  where 
    nextPiv = getPivot point b
    up = getTracks nextPiv UP
    down = getTracks nextPiv DOWN
    left = getTracks nextPiv LEFT
    right = getTracks nextPiv RIGHT
    validDirs = dumbShuffle (filter (/= Null) [up,  left, down, right ])
moveBill (Ghost loc (dest, t:ts) curr next d u) _ = Ghost t (dest, ts) curr next d u



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




