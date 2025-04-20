module Board where

-- data Grid = [(Zone, Node)]
--     deriving (Eq, Show)
data Board = Board [Tile]
    deriving (Eq, Show)   
data Direction = UP | DOWN | LEFT | RIGHT | NONE
    deriving (Enum, Eq, Show)
data Tile = Tile (Zone, Node)
    deriving (Eq, Show)
data Point = Point (Float, Float)
    deriving (Eq, Show)
data Node = None | Wall | Path Center Zone [Direction]
    deriving (Eq, Show)
-- data Node = Wall | Path Center Zone (Neighbors [(Direction, Node)])
--     deriving (Eq, Show)
data Center = Center Point
    deriving (Eq, Show)
data Zone = Zone Float Float Float Float
    deriving (Eq, Show)
data Neighbors = Neighbors [(Direction, Node)]
    deriving (Eq, Show)


genLevel :: Int -> Board
genLevel lvlNum = Board (genGrid lvlNum getTopRightCors)

getTopRightCors :: [Point]
getTopRightCors = [Point (x, y) | x <- [1..(getLength)], y <- [1..(getHeight)]]

-- give me the level number and top right corners of the inner walls
genGrid :: Int -> [Point] -> [Tile]
genGrid _ [] = []
genGrid lvlNum (p:pts)
    | isBoundary p walls = (genTile zone Wall) : genGrid lvlNum pts-- this is a wall
    | otherwise = (genTile zone node) : genGrid lvlNum pts-- this is a valid pathway
    where 
        walls = getWalls lvlNum
        zone = constructZone p
        node = constructNode p walls
    -- we need to connect it to the surrounding non nodes

genTile :: Zone -> Node -> Tile
genTile z n = Tile (z, n)

getWalls :: Int -> [Point]
getWalls lvlNum 
    | lvlNum == 1 = lvl1Walls -- todo, more levels
    | otherwise = lvl1Walls

-- hardcoded for now
lvl1Walls :: [Point]
lvl1Walls =  [Point (1, 2), Point (3, 3), Point (3, 2)]

constructZone :: Point -> Zone
constructZone (Point (x, y)) = Zone (offset (x - 1)) (offset x) (offset (y - 1)) (offset y)

constructNode :: Point -> [Point] -> Node
constructNode p walls = Path (getCenter p) (constructZone p) (possibleDirections p walls)

getCenter :: Point -> Center
getCenter (Point (x, y)) = Center (Point (offset x - 50, offset y - 50)) -- very simple for now

possibleDirections :: Point -> [Point] -> [Direction]
possibleDirections p walls  = filter (\d -> d /= NONE) ds
    where 
        ds = (up p walls) : (down p walls) : (left p walls) : (right p walls) : []
    
getNode :: Point -> Board -> Node
getNode _ (Board []) = None
getNode (Point (x, y)) (Board ((Tile (Zone x1 x2 y1 y2, node)):tiles))
    | and [xStart >= x1 + 20, xEnd <= x2 - 20, yStart >= y1 + 20, yEnd <= y2 - 20] = node
    | otherwise = getNode (Point (x, y)) (Board tiles)
    where
        xStart = fromInteger (floor  x)
        xEnd = fromInteger (ceiling x)
        yStart = fromInteger (floor y)
        yEnd = fromInteger (ceiling y)

getDirs :: Node -> [Direction]
getDirs (Path _ _ d) = d
getDirs _ = []

up :: Point -> [Point] -> Direction
up (Point (x, y)) walls
    | isBoundary upP walls = NONE
    | otherwise = UP
    where 
        upP = Point (x, y + 1)

down :: Point -> [Point] -> Direction
down (Point (x, y)) walls
    | isBoundary downP walls = NONE
    | otherwise = DOWN
    where 
        downP = Point (x, y - 1)

left :: Point -> [Point] -> Direction
left (Point (x, y)) walls
    | isBoundary leftP walls = NONE
    | otherwise = LEFT
    where 
        leftP = Point (x - 1, y)

offset :: Float -> Float
offset x = x * 50 - 100

right :: Point -> [Point] -> Direction
right (Point (x, y)) walls
    | isBoundary rightP walls = NONE
    | otherwise = RIGHT
    where 
        rightP = Point (x + 1, y)


isBoundary :: Point -> [Point] -> Bool
isBoundary (Point (x, y)) walls = 
    elem (Point (x, y)) walls 
    || x == 0 || x > getLength
    || y == 0 || y > getHeight

getHeight :: Float
getHeight = 4

getLength :: Float
getLength = 4




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