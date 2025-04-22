module Board where

import Brillo
-- data Grid = [(Zone, Node)]
--     deriving (Eq, Show)
-- data Board = Board [Tile]
--     deriving (Eq, Show)   

-- data Tile = Tile (Zone, Node)
--     deriving (Eq, Show)

-- data Node = None | Wall | Path Center Zone [Direction]
--     deriving (Eq, Show)
-- -- data Node = Wall | Path Center Zone (Neighbors [(Direction, Node)])
-- --     deriving (Eq, Show)
-- data Center = Center Point
--     deriving (Eq, Show)
-- data Zone = Zone Float Float Float Float
--     deriving (Eq, Show)
-- data Neighbors = Neighbors [(Direction, Node)]
--     deriving (Eq, Show)

data Board = 
  Board {
    tiles :: [Tile], -- for display
    pivots :: [Pivot] -- for actual movement
  }
  deriving (Eq, Show) 

data Tile = Tile Color Boundary -- purely for drawing the tile map, aesthetics alone
  deriving (Eq, Show) 

-- up down left right
data Pivot = Pivot Point (Neighbor, Neighbor, Neighbor, Neighbor)
  deriving (Eq, Show)  

data Neighbor = Null | Neighbor Destination [Track]
  deriving (Eq, Show) 

data Destination = None | Destination Point
  deriving (Eq, Show) 
-- data Track = Track Point 
--   deriving (Eq, Show) 

data Boundary = 
  Boundary {
    b :: Bottom,
    t :: Top,
    l :: Left,
    r :: Right
  }
  deriving (Eq, Show) 

type Bottom = Float -- for ease of reading lol
type Top = Float
type Left = Float
type Right = Float
type Track = Point

data Direction = UP | DOWN | LEFT | RIGHT | NONE
    deriving (Enum, Eq, Show)


playerStartPoint :: Point
playerStartPoint = (-475, -475)

genLevel :: Int -> Board
genLevel lvlNum 
  | lvlNum == 1 = Board (genTiles lvl1Walls) (genPivots lvl1Walls)
  | otherwise = Board [] []


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
genPivots walls = [ Pivot p (getNeighbor UP p walls, getNeighbor DOWN p walls, getNeighbor LEFT p walls, getNeighbor RIGHT p walls) | p <- genCenters, not (elem p walls)]

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
  | dir == UP = go (>=) start end [] 0 0.1
  | dir == DOWN = go (<=) start end [] 0 (-0.1)
  | dir == LEFT = go (<=) start end [] (-0.1) 0
  | dir == RIGHT = go (>=) start end [] 0.1 0
  | otherwise = []
  where 
    go pred (x1, y1) (x2, y2) acc xAcc yAcc
      | pred x1 x2 && pred y1 y2 = reverse ((x2, y2) : acc) -- issue with works for up/right i think, potential issue down left
      | otherwise = go pred (x1 + xAcc, y1 + yAcc) (x2, y2) ((x1 + xAcc, y1 + yAcc) : acc) xAcc yAcc


getPivot :: Point -> Board -> Maybe Pivot
getPivot _ (Board _ []) = Nothing
getPivot point (Board ts ((Pivot pt ns):ps))
  | point == pt = Just (Pivot pt ns)
  | otherwise = getPivot point (Board ts ps)

{-
------------------------------------------------------------
WALLS

specify walls by the center point only.
------------------------------------------------------------
-}

lvl1Walls :: [Point]
lvl1Walls =  [(-375, -375), (175, 175), (25, 25)]

outOfBounds :: Point -> Bool
outOfBounds (x, y) = x > 500 || x < -500 || y > 250 || y < -500

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