module Main where

import Brillo
import Brillo.Interface.IO.Game
import Board
import Ghost
import Player


data MainGameWorld = 
    MainGameWorld {
      board :: Board,
      player :: Player,
      ghosts :: [Ghost]
    }
    deriving (Eq, Show)



main :: IO ()
main = 
    play 
      window
      white
      75
      world
      draw
      handle            
      update



draw :: MainGameWorld -> Picture
draw (MainGameWorld b p gs) = Pictures (drawBoard b ++ (drawPlayer p : drawGhosts gs)) -- tiles, collectibles, then player, ghosts

update :: Float -> MainGameWorld -> MainGameWorld 
update _ w = updateWorld w
-- update _ (MainGameWorld b (Player (x, y) a f c d) stuff) = (MainGameWorld b (Player (x + 1, y) a f c d) stuff)

handle :: Event -> MainGameWorld -> MainGameWorld
handle (EventKey (SpecialKey KeyUp) Down _ _) w = queueMove w UP
handle (EventKey (SpecialKey KeyDown) Down _ _) w = queueMove w DOWN
handle (EventKey (SpecialKey KeyLeft) Down _ _) w = queueMove w LEFT
handle (EventKey (SpecialKey KeyRight) Down _ _) w = queueMove w RIGHT
handle _ w = w

queueMove :: MainGameWorld -> Direction -> MainGameWorld
queueMove (MainGameWorld b (Player l path curr _) gs) dir = MainGameWorld b (Player l path curr dir) gs

world :: MainGameWorld
world = MainGameWorld (genLevel 1) genPlayer []



window :: Display
window = InWindow "HaskMan" (1050, 1050) (0, 0)


{-
------------------------------------------------------------
DRAWING FUNCTIONS
------------------------------------------------------------
-}

drawBoard :: Board -> [Picture]
drawBoard (Board ts ps l s) = drawScore s : drawLives l : drawGrid ts

drawGrid :: [Tile] -> [Picture]
drawGrid  [] = [] 
drawGrid (t:ts) = drawTile t : drawBorder t : drawGrid ts

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
drawPlayer (Player (x, y) _ _ _) = color yellow (translate x y (thickCircle 10 20))

drawGhosts :: [Ghost] -> [Picture]
drawGhosts gs = [Circle 15] -- todo


{-
------------------------------------------------------------
UPDATING FUNCTIONS
------------------------------------------------------------
-}

updateWorld :: MainGameWorld -> MainGameWorld
updateWorld (MainGameWorld b p gs) = MainGameWorld (updateBoard b) (updatePlayer p b) (updateGhosts gs)




-- handleEvent :: Event -> World -> World
-- handleEvent (EventKey (SpecialKey KeyUp) Down _ _) w = tryMove w UP
-- handleEvent (EventKey (SpecialKey KeyDown) Down _ _) w = tryMove w DOWN
-- handleEvent (EventKey (SpecialKey KeyLeft) Down _ _) w = tryMove w LEFT
-- handleEvent (EventKey (SpecialKey KeyRight) Down _ _) w = tryMove w RIGHT
-- handleEvent _ w = w

-- updateFunc :: Float -> World -> World
-- updateFunc t (World (Player (x, y) dir next (vx, vy)) b)
--   | dir /= next = move (World (Player (x, y) dir next (vx, vy)) b) next t
--   | otherwise = move (World (Player (x, y) dir next (vx, vy)) b) dir t

-- drawFunc :: World -> Picture
-- drawFunc world = Pictures (drawGame world)

-- drawGame :: World -> [Picture]
-- drawGame (World p b) = drawBoard b ++ ((Color black (translate 500 500 (Circle 10))): (Color black ((Circle 10))) : drawPlayer p : [] ) 
-- getNodePics w = drawPlayer w : genCircles (getPoints w)

-- drawPlayer :: Player -> Picture -- todo no offset for now
-- drawPlayer (Player (x, y) _ _ _) = Color black ((translate x y) (Circle 15))
--  where
--     a = x * 100 - 150 -- off set not used right now
--     b = y * 100 - 150

-- drawBoard :: Board -> [Picture]
-- drawBoard (Board []) = [] 
-- drawBoard (Board (t:ts)) = drawTile t : drawBoard (Board (ts))





-- genCircles :: [(Int, Int)] -> [Picture]
-- genCircles [] = []
-- genCircles ((x, y):pts) = Color red (translate a b (Circle 20)) : genCircles pts
--   where
--     a = fromIntegral (x * 100 - 150)
--     b = fromIntegral (y * 100 - 150)

-- getPoints :: World -> [(Int, Int)]
-- getPoints (World _ _ _ (Grid points)) = points


-- data Direction = UP | DOWN | LEFT | RIGHT | NONE
--   deriving (Enum, Eq, Show)

-- getGrid :: Int -> Int -> [(Int, Int)]
-- getGrid l w = [(x, y) | x <- [0..l], y <- [0..w]]


-- tryMove :: World -> Direction -> World
-- tryMove (World (Player pos curr next v) b) dir =   
--   (World (Player pos curr dir v) b)

-- move :: World -> Direction -> Float -> World
-- move (World (Player (x, y) curr next (vx, vy)) b) dir t
--   | validMove (Player (x, y) curr next (vx, vy)) dir b = (World (Player (dx, dy) dir next (getVelocity dir)) b)
--   | otherwise = (World (Player (dx, dy) curr next (vx, vy)) b) -- do nothing
--   where 
--     dx = x + vx * 0.01
--     dy = y + vy * 0.01

-- move (World (Player (x, y) curr next (vx, vy)) b) dir t =
--   (World (Player (dx, dy) dir next (getVelocity dir)) b)
--   where 
--     dx = x + vx * t
--     dy = y + vy * t

{- 
we want to 
(1) see where the player is: Zone -> Node
(2) see if that's a valid direction from there
-}
-- validMove :: Player -> Direction -> Board -> Bool
-- validMove (Player (x, y) _ _ _) dir b =
--   -- do 
--     -- something dirs
--     elem dir dirs 
--   where 
--     currNode = getNode (Point (x, y)) b
--     dirs = getDirs currNode

-- something :: [Direction] -> IO()
-- something dirs = print dirs 

-- getVelocity :: Direction -> (Float, Float)
-- getVelocity dir
--   | dir == UP    = (0.0,  85.0)
--   | dir == DOWN  = (0.0, -85.0)
--   | dir == LEFT  = (-85.0, 0.0)
--   | dir == RIGHT = ( 85.0, 0.0)
--   | otherwise = (0, 0)

-- specialKeyPressed :: Event -> Bool
-- specialKeyPressed (EventKey (Char k) _ _ _) = k == 'g'
-- specialKeyPressed _ = False


