module Main where

import Brillo
import Brillo.Interface.IO.Game
import Board
import Ghost
import Player
import Data


-- functions in order: draw, update, handle (d u h)
data World = 
    TitleScreen (World -> Picture) (Float -> World -> World) (Event -> World -> World)
  | MainGameWorld Board (World -> Picture) (Float -> World -> World) (Event -> World -> World)




main :: IO ()
main = 
    play 
      window
      white
      75
      getTitle
      draw
      handle            
      update


draw :: World -> Picture 
draw (TitleScreen d u h) = d (TitleScreen d u h)
draw (MainGameWorld b d u h) = d (MainGameWorld b d u h)
draw _ = Blank

update :: Float -> World -> World
update t (TitleScreen d u h) = u t (TitleScreen d u h)
update t (MainGameWorld b d u h) = u t (MainGameWorld b d u h)
update _ w = w


handle :: Event -> World -> World
handle e (TitleScreen d u h) = h e (TitleScreen d u h)
handle e (MainGameWorld b d u h) = h e (MainGameWorld b d u h)
handle _ w = w


{-
------------------------------------------------------------
MAIN GAME FUNCTIONS
------------------------------------------------------------
-}

drawMain :: World -> Picture
drawMain (MainGameWorld (Board ts ps cs ls s dB uB p gs gOver timers) _ _ _) = 
  Pictures (dB (Board ts ps cs ls s dB uB p gs gOver timers)) -- tiles, collectibles, then player, ghosts
drawMain _ = Blank

updateMain :: Float -> World -> World 
updateMain dt (MainGameWorld (Board ts ps cs ls s dB uB p gs gOver timers) d u h ) = 
  MainGameWorld (uB (Board ts ps cs ls s dB uB p gs gOver timers) dt) d u h 
updateMain _ w = w

handleMain :: Event -> World -> World
handleMain (EventKey (Char 'g') Down _ _) (MainGameWorld (Board ts ps cs ls s dB uB p gs gOver timers) d u h) 
  | gOver = getTitle -- todo: works! leave this here to know implementation of main title thing
  | otherwise = MainGameWorld (Board ts ps cs ls s dB uB p gs gOver timers) d u h
handleMain (EventKey (SpecialKey KeyUp) Down _ _) w = queueMove w UP
handleMain (EventKey (SpecialKey KeyDown) Down _ _) w = queueMove w DOWN
handleMain (EventKey (SpecialKey KeyLeft) Down _ _) w = queueMove w LEFT
handleMain (EventKey (SpecialKey KeyRight) Down _ _) w = queueMove w RIGHT
handleMain _ w = w

queueMove :: World -> Direction -> World -- move t player?
queueMove (MainGameWorld (Board ts ps cs ls s dB uB (Player l path curr _ dp up coll) gs gOver timers) d u h) dir =
   MainGameWorld (Board ts ps cs ls s dB uB (Player l path curr dir dp up coll) gs gOver timers) d u h
queueMove w _ = w

{-
------------------------------------------------------------
TITLE SCREEN FUNCTIONS
------------------------------------------------------------
-}

updateTitle :: Float -> World -> World
updateTitle _ w = w 

drawTitle :: World -> Picture
drawTitle (TitleScreen _ _ _) = Pictures [
  scale 0.5 0.5 (translate (-700) 0 (Text "Welcome to HaskMan!")),
  scale 0.25 0.25 (translate (-700) (-300) (Text "Press 1 for Level 1.")),
  scale 0.25 0.25 (translate (-700) (-600) (Text "Press 2 for Level 2.")),
  scale 0.25 0.25 (translate (-700) (-900) (Text "Press 3 for Level 3."))
  ]
drawTitle _ = Blank

handleTitle :: Event -> World -> World
handleTitle (EventKey (Char '1') Down _ _) w = getLevel1
handleTitle _ w = w

{-
------------------------------------------------------------
DISPLAY FUNCTIONS
------------------------------------------------------------
-}

getLevel1 :: World
getLevel1 = MainGameWorld (genLevel 1) drawMain updateMain handleMain

getTitle :: World 
getTitle = TitleScreen drawTitle updateTitle handleTitle

window :: Display
window = InWindow "HaskMan" (1050, 1050) (0, 0)








