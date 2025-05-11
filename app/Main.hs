module Main where

import Brillo
import Brillo.Interface.IO.Game
import Board
import Ghost
import Player
import Data


{-
  @author Roxanne Lutz
  the main functionality to keep the game running.
-}

-- what a "world" is, in our case. can be either menu or title screen
data World = 
    TitleScreen (World -> Picture) (Float -> World -> World) (Event -> World -> World)
  | MainGameWorld Board (World -> Picture) (Float -> World -> World) (Event -> World -> World)

{-
------------------------------------------------------------
MAIN FUNCTION
------------------------------------------------------------
-}

main :: IO ()
main = 
    play 
      window
      white
      defaultSimCount
      getTitle
      draw
      handle            
      update

{-
------------------------------------------------------------
FUNCTIONS FOR DRAW, UPDATE, HANDLE WORLD
------------------------------------------------------------
-}

-- entry point to draw based on world type
draw :: World -> Picture 
draw (TitleScreen d u h) = d (TitleScreen d u h)
draw (MainGameWorld b d u h) = d (MainGameWorld b d u h)
draw _ = Blank

-- entry point to update based on world type
update :: Float -> World -> World
update t (TitleScreen d u h) = u t (TitleScreen d u h)
update t (MainGameWorld b d u h) = u t (MainGameWorld b d u h)
update _ w = w

-- entry point to handle based on world type
handle :: Event -> World -> World
handle e (TitleScreen d u h) = h e (TitleScreen d u h)
handle e (MainGameWorld b d u h) = h e (MainGameWorld b d u h)
handle _ w = w

{-
------------------------------------------------------------
MAIN GAME FUNCTIONS
------------------------------------------------------------
-}

-- call the draw function from the main game
drawMain :: World -> Picture
drawMain (MainGameWorld (Board ts ps cs ls s dB uB p gs gOver timers) _ _ _) = 
  Pictures (dB (Board ts ps cs ls s dB uB p gs gOver timers)) 
drawMain _ = Blank

-- call the update function from the main game
updateMain :: Float -> World -> World 
updateMain dt (MainGameWorld (Board ts ps cs ls s dB uB p gs gOver timers) d u h ) = 
  MainGameWorld (uB (Board ts ps cs ls s dB uB p gs gOver timers) dt) d u h 
updateMain _ w = w

-- handle function for main game, if more needed, would be treated as ^, but easier to keep here for now.
handleMain :: Event -> World -> World
handleMain (EventKey (Char 'g') Down _ _) (MainGameWorld (Board ts ps cs ls s dB uB p gs gOver timers) d u h) 
  | gOver = getTitle
  | otherwise = MainGameWorld (Board ts ps cs ls s dB uB p gs gOver timers) d u h
handleMain (EventKey (SpecialKey KeyUp) Down _ _) w = queueMove w UP
handleMain (EventKey (Char 'w') Down _ _) w = queueMove w UP
handleMain (EventKey (SpecialKey KeyDown) Down _ _) w = queueMove w DOWN
handleMain (EventKey (Char 's') Down _ _) w = queueMove w DOWN
handleMain (EventKey (SpecialKey KeyLeft) Down _ _) w = queueMove w LEFT
handleMain (EventKey (Char 'a') Down _ _) w = queueMove w LEFT
handleMain (EventKey (SpecialKey KeyRight) Down _ _) w = queueMove w RIGHT
handleMain (EventKey (Char 'd') Down _ _) w = queueMove w RIGHT
handleMain _ w = w

-- queue up a move for the player, return next state of world
queueMove :: World -> Direction -> World 
queueMove (MainGameWorld (Board ts ps cs ls s dB uB (Player l path curr _ dp up coll) gs gOver timers) d u h) dir =
   MainGameWorld (Board ts ps cs ls s dB uB (Player l path curr dir dp up coll) gs gOver timers) d u h
queueMove w _ = w

{-
------------------------------------------------------------
TITLE SCREEN FUNCTIONS
------------------------------------------------------------
-}

-- function to draw the title screen
drawTitle :: World -> Picture
drawTitle (TitleScreen _ _ _) = Pictures [
  scale 0.5 0.5 (translate (-700) 0 (Text "Welcome to HaskMan!")),
  scale 0.25 0.25 (translate (-700) (-300) (Text "Press 1 for Level 1.")),
  scale 0.25 0.25 (translate (-700) (-600) (Text "Press 2 for Level 2.")),
  scale 0.25 0.25 (translate (-700) (-900) (Text "Press 3 for Level 3."))
  ]
drawTitle _ = Blank

-- no update needed, is just title screen
updateTitle :: Float -> World -> World
updateTitle _ w = w 

-- function to handle input at title 
handleTitle :: Event -> World -> World
handleTitle (EventKey (Char '1') Down _ _) w = getLevel1
handleTitle _ w = w

{-
------------------------------------------------------------
DISPLAY AND WORLD FUNCTIONS
------------------------------------------------------------
-}

-- get me the first level
getLevel1 :: World
getLevel1 = MainGameWorld (genLevel 1) drawMain updateMain handleMain

-- get me the title screen
getTitle :: World 
getTitle = TitleScreen drawTitle updateTitle handleTitle

-- get me the player window
window :: Display
window = InWindow "HaskMan" (1050, 1050) (0, 0)

-- constant simulation count
defaultSimCount :: Int
defaultSimCount = 75
