module Board where

import Brillo
import Data
-- import Ghost
-- import Player

updateBoard :: Board -> Board
updateBoard (Board ts ps cs ls s dB uB p gs) = 
  do
    let updatedP = updatePlayer p (Board ts ps cs ls s dB uB p gs)
    let updatedGs = updateGhosts gs (Board ts ps cs ls s dB uB p gs)
    checkCollision (updateCollectibles (Board ts ps cs ls s dB uB updatedP updatedGs)) updatedP updatedGs



checkCollision :: Board -> Player -> [Ghost] -> Board
checkCollision b p [] = b
checkCollision 
  (Board ts ps cs lives s dB uB op ogs)
  (Player locP desP currP nextP dP uP coll) 
  ((Ghost locG desG currG nextG dG uG):gs)
    | coll = if collisionDetected locP locG then chugBeer (Board ts ps cs lives s dB uB op ogs) else checkCollision (Board ts ps cs lives s dB uB op ogs) (Player locP desP currP nextP dP uP coll) gs
    | otherwise = Board ts ps cs lives s dB uB op ogs
 
chugBeer :: Board -> Board
chugBeer (Board ts ps cs lives s dB uB op ogs) = Board ts ps cs (lives - 1) s dB uB genPlayer genGhosts

collisionDetected :: Point -> Point -> Bool
collisionDetected (x1, y1) (x2, y2)
  | sameCol (x1, y1) (x2, y2) = max y1 y2 - min y1 y2 < 40
  | sameRow (x1, y1) (x2, y2) = max x1 x2 - min x1 x2 < 40
  | otherwise = False
  
sameCol :: Point -> Point -> Bool
sameCol (x1, _) (x2, _) = x1 == x2

sameRow :: Point -> Point -> Bool
sameRow (_, y1) (_, y2) = y1 == y2
  


-- terribly inefficient, poorly organized, this would be better encapsed in player some how...but idk yet
updateCollectibles :: Board -> Board 
updateCollectibles (Board ts ps cs l s d u (Player loc dest curr next dp up coll) gs) 
  | length filteredColls < length cs = Board ts ps (Eaten loc : filteredColls) l (s + updateScore (findColl loc cs)) d u (Player loc dest curr next dp up coll) gs
  | otherwise = Board ts ps cs l s d u (Player loc dest curr next dp up coll) gs
  where
    filteredColls = filter (\c -> deconCollLoc c /= loc) cs

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
getPivot _ (Board _ [] _ _ _ _ _ _ _) = Nothing
getPivot point (Board ts ((Pivot pt ns):ps) cs l s d u p gs)
  | point == pt = Just (Pivot pt ns)
  | otherwise = getPivot point (Board ts ps cs l s d u p gs) 

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
playerInitLives = 3

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
  | lvlNum == 1 = Board (genTiles lvl1Walls) (genPivots lvl1Walls) (genCollectibles (playerStartPoint:lvl1Walls)) playerInitLives playerInitScore drawBoard updateBoard genPlayer genGhosts
  | otherwise = Board [] [] [] 0 0 drawBoard updateBoard genPlayer genGhosts

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
drawBoard (Board ts ps cs l s d u p gs) = drawScore s : drawLives l :  (drawGrid ts ++ drawCollectibles cs) ++ (drawPlayer p : drawGhosts gs)

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





-- getTopRightCors :: [Point]
-- getTopRightCors = [Point (x, y) | x <- [1..(getLength)], y <- [1..(getHeight)]]

-- -- give me the level number and top right corners of the inner walls
-- genGrid :: Int -> [Point] -> [Tile]
-- genGrid _ [] = []
-- genGrid lvlNum (p:pts)
--     | isBoundary p walls = (genTile zone Wall) : genGrid lvlNum pts-- this is a wall
--     | otherwise = (genTile zone node) : genGrid lvlNum pts-- this is a valid pathway
--     where 
--         walls = getWalls lvlNum
--         zone = constructZone p
--         node = constructNode p walls
--     -- we need to connect it to the surrounding non nodes

-- genTile :: Zone -> Node -> Tile
-- genTile z n = Tile (z, n)

-- getWalls :: Int -> [Point]
-- getWalls lvlNum 
--     | lvlNum == 1 = lvl1Walls -- todo, more levels
--     | otherwise = lvl1Walls



-- constructZone :: Point -> Zone
-- constructZone (Point (x, y)) = Zone (offset (x - 1)) (offset x) (offset (y - 1)) (offset y)

-- constructNode :: Point -> [Point] -> Node
-- constructNode p walls = Path (getCenter p) (constructZone p) (possibleDirections p walls)

-- getCenter :: Point -> Center
-- getCenter (Point (x, y)) = Center (Point (offset x - 50, offset y - 50)) -- very simple for now

-- possibleDirections :: Point -> [Point] -> [Direction]
-- possibleDirections p walls  = filter (\d -> d /= NONE) ds
--     where 
--         ds = (up p walls) : (down p walls) : (left p walls) : (right p walls) : []
    
-- getNode :: Point -> Board -> Node
-- getNode _ (Board []) = None
-- getNode (Point (x, y)) (Board ((Tile (Zone x1 x2 y1 y2, node)):tiles))
--     | and [xStart >= x1 + 20, xEnd <= x2 - 20, yStart >= y1 + 20, yEnd <= y2 - 20] = node
--     | otherwise = getNode (Point (x, y)) (Board tiles)
--     where
--         xStart = fromInteger (floor  x)
--         xEnd = fromInteger (ceiling x)
--         yStart = fromInteger (floor y)
--         yEnd = fromInteger (ceiling y)

-- getDirs :: Node -> [Direction]
-- getDirs (Path _ _ d) = d
-- getDirs _ = []

-- up :: Point -> [Point] -> Direction
-- up (Point (x, y)) walls
--     | isBoundary upP walls = NONE
--     | otherwise = UP
--     where 
--         upP = Point (x, y + 1)

-- down :: Point -> [Point] -> Direction
-- down (Point (x, y)) walls
--     | isBoundary downP walls = NONE
--     | otherwise = DOWN
--     where 
--         downP = Point (x, y - 1)

-- left :: Point -> [Point] -> Direction
-- left (Point (x, y)) walls
--     | isBoundary leftP walls = NONE
--     | otherwise = LEFT
--     where 
--         leftP = Point (x - 1, y)

-- offset :: Float -> Float
-- offset x = x * 50 - 100

-- right :: Point -> [Point] -> Direction
-- right (Point (x, y)) walls
--     | isBoundary rightP walls = NONE
--     | otherwise = RIGHT
--     where 
--         rightP = Point (x + 1, y)


-- isBoundary :: Point -> [Point] -> Bool
-- isBoundary (Point (x, y)) walls = 
--     elem (Point (x, y)) walls 
--     || x == 0 || x > getLength
--     || y == 0 || y > getHeight

-- getHeight :: Float
-- getHeight = 4

-- getLength :: Float
-- getLength = 4




-- --                      (up,    down,  left,  right)
-- data Graph = Nil | Node (Graph, Graph, Graph, Graph) Int Int
--     deriving (Eq, Show)

-- take in (1) the x size, (2) y size, return a list of iterative coordinates
-- getCoords :: Int -> Int -> [(Int, Int)]
-- getCoords l w = [(x, y) | x <- [0..l], y <- [0..w]]

-- getNodes ::  [(Int, Int)] -> [Graph]
-- getNodes = map go
--     where 
--         go (x, y) = Node (Nil, Nil, Nil, Nil) x y

-- connectNodes :: [Graph] -> [Graph]
-- connectNodes nodes = connectRows nodes
--     -- where 
--         -- r =  
--         -- l = connectLeft r
--         -- u = connectUp l
--         -- finished = connectDown u

-- connectRows :: [Graph] -> [Graph]
-- -- connectRight ((Node (u1, d1, l1, r1) x1 y1) : (Node (u2, d2, l2, r2) x2 y2) : nodes) 
-- --     | 

-- connectRows (n:nodes) = connL (connR sameRows)
--     where 
--         sameRows = allNodesInRow (getRow n) (n:nodes)
--         -- ascending = sortAscending sameRows

-- connR :: [Graph] -> [Graph]
-- connR [n] = [n]
-- connR ((Node (u1, d1, l1, r1) x1 y1):(Node (u2, d2, l2, r2) x2 y2):nodes) =
--     (Node (u1, d1, l1, (Node (u2, d2, l2, r2) x2 y2)) x1 y1) : connR (((Node (u2, d2, l2, r2) x2 y2)): nodes)

-- connL :: [Graph] -> [Graph]
-- connL [n] = [n]
-- connL ((Node (u1, d1, l1, r1) x1 y1):(Node (u2, d2, l2, r2) x2 y2):nodes) =
--     (Node (u1, d1, l1, r2) x1 y1) 
--     : (Node (u2, d2, (Node (u1, d1, l1, r2) x1 y1) , r2) x2 y2)
--     : connR ((Node (u2, d2, (Node (u1, d1, l1, r2) x1 y1) , r2) x2 y2): nodes)

-- connectLeft :: [Graph] -> [Graph]
-- connectLeft = undefined

-- connectUp :: [Graph] -> [Graph]
-- connectUp = undefined

-- connectDown :: [Graph] -> [Graph]
-- connectDown = undefined

-- allNodesInRow :: Int -> [Graph] -> [Graph]
-- allNodesInRow row nodes = filter pred nodes
--     where 
--         pred (Node (_) x y) = y == row

-- allNodesInCol :: Int -> [Graph] -> [Graph]
-- allNodesInCol col nodes = filter pred nodes
--     where 
--         pred (Node (_) x y) = x == col

-- getRow :: Graph -> Int
-- getRow (Node (_) r _) = r

-- getCol :: Graph -> Int
-- getCol (Node (_) _ c) = c
-- -- genPath