module Main where

import Brillo
import Brillo.Interface.IO.Game
import Board
import Ghost
import Player

main :: IO ()
main = 
  do
    let window = mainWindow
    let world = getWorld
    play 
      window
      white
      100
      world
      drawFunc
      handleEvent            
      updateFunc
    -- play 
    --   window
    --   white
    --   100
    --   world
    --   -- (\world -> translate 50 50 world)
    --   id
    --   handleEvent            
    --   (\_ cir -> cir)

    -- animate 
    --   window
    --   white 
    --   (\t -> getWorld t)

    -- simulate
    --   window 
    --   white
    --   100
    --   getWorld
    --   id
    --   (\vp x m -> m)

updateFunc :: Float -> World -> World
updateFunc t (World (Player (x, y) dir next (vx, vy)) b)
  | dir /= next = move (World (Player (x, y) dir next (vx, vy)) b) next t
  | otherwise = move (World (Player (x, y) dir next (vx, vy)) b) dir t

drawFunc :: World -> Picture
drawFunc world = Pictures (drawGame world)

drawGame :: World -> [Picture]
drawGame (World p b) = drawBoard b ++ ((Color black (translate 500 500 (Circle 10))): (Color black ((Circle 10))) : drawPlayer p : [] ) 
-- getNodePics w = drawPlayer w : genCircles (getPoints w)

drawPlayer :: Player -> Picture -- todo no offset for now
drawPlayer (Player (x, y) _ _ _) = Color black ((translate x y) (Circle 15))
 where
    a = x * 100 - 150 -- off set not used right now
    b = y * 100 - 150

drawBoard :: Board -> [Picture]
drawBoard (Board []) = [] 
drawBoard (Board (t:ts)) = drawTile t : drawBoard (Board (ts))

drawTile :: Tile -> Picture
drawTile (Tile ((Zone xStart1 xEnd1 yStart1 yEnd1), Wall)) = 
  Color blue ((Polygon [(xStart1, yStart1), (xEnd1, yStart1), (xEnd1, yEnd1), (xStart1, yEnd1)]))
drawTile (Tile ((Zone xStart2 xEnd2 yStart2 yEnd2), _)) = 
  Color red ((Polygon [(xStart2, yStart2), (xEnd2, yStart2), (xEnd2, yEnd2), (xStart2, yEnd2)]))



genCircles :: [(Int, Int)] -> [Picture]
genCircles [] = []
genCircles ((x, y):pts) = Color red (translate a b (Circle 20)) : genCircles pts
  where
    a = fromIntegral (x * 100 - 150)
    b = fromIntegral (y * 100 - 150)

-- getPoints :: World -> [(Int, Int)]
-- getPoints (World _ _ _ (Grid points)) = points

data Grid = Grid [(Int, Int)]
  deriving (Show)

data World = World {
  player :: Player,
  board :: Board
}
  deriving (Show)

data Player = Player {
  playerLocation :: (Float, Float),
  currDirection :: Direction, 
  nextDirection :: Direction, 
  velocity :: (Float, Float)
}
  deriving (Show)

-- data Direction = UP | DOWN | LEFT | RIGHT | NONE
--   deriving (Enum, Eq, Show)

getWorld :: World
getWorld = World  (Player (-75, -75) NONE NONE (0, 0)) (genLevel 1)

-- getGrid :: Int -> Int -> [(Int, Int)]
-- getGrid l w = [(x, y) | x <- [0..l], y <- [0..w]]

handleEvent :: Event -> World -> World
handleEvent (EventKey (SpecialKey KeyUp) Down _ _) w = tryMove w UP
handleEvent (EventKey (SpecialKey KeyDown) Down _ _) w = tryMove w DOWN
handleEvent (EventKey (SpecialKey KeyLeft) Down _ _) w = tryMove w LEFT
handleEvent (EventKey (SpecialKey KeyRight) Down _ _) w = tryMove w RIGHT
handleEvent _ w = w

tryMove :: World -> Direction -> World
tryMove (World (Player pos curr next v) b) dir =   
  (World (Player pos curr dir v) b)

move :: World -> Direction -> Float -> World
move (World (Player (x, y) curr next (vx, vy)) b) dir t
  | validMove (Player (x, y) curr next (vx, vy)) dir b = (World (Player (dx, dy) dir next (getVelocity dir)) b)
  | otherwise = (World (Player (dx, dy) curr next (vx, vy)) b) -- do nothing
  where 
    dx = x + vx * 0.01
    dy = y + vy * 0.01

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
validMove :: Player -> Direction -> Board -> Bool
validMove (Player (x, y) _ _ _) dir b =
  -- do 
    -- something dirs
    elem dir dirs 
  where 
    currNode = getNode (Point (x, y)) b
    dirs = getDirs currNode

something :: [Direction] -> IO()
something dirs = print dirs 

getVelocity :: Direction -> (Float, Float)
getVelocity dir
  | dir == UP    = (0.0,  85.0)
  | dir == DOWN  = (0.0, -85.0)
  | dir == LEFT  = (-85.0, 0.0)
  | dir == RIGHT = ( 85.0, 0.0)
  | otherwise = (0, 0)

specialKeyPressed :: Event -> Bool
specialKeyPressed (EventKey (Char k) _ _ _) = k == 'g'
specialKeyPressed _ = False

mainWindow :: Display
mainWindow = InWindow "Nice Window" (1000, 1000) (100, 100)
