module Ghost where

import Brillo
import Board
import Data

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
genHank = Ghost (475, 225) (None, []) NONE NONE drawHank updateHank

genDale :: Ghost
genDale = Ghost (425, 225) (None, []) NONE NONE drawDale updateDale

genBoomhauer :: Ghost
genBoomhauer = Ghost (375, 225) (None, []) NONE NONE drawBoomhauer updateBoomhauer

genBill :: Ghost
genBill = Ghost (325, 225) (None, []) NONE NONE drawBill updateBill


{-
------------------------------------------------------------
UPDATE FUNCTIONS
------------------------------------------------------------
-}

updateGhosts :: [Ghost] -> [Ghost]
updateGhosts gs board = gs

updateHank :: Ghost -> Board -> Ghost
updateHank g board = g

updateDale :: Ghost -> Board -> Ghost
updateDale g board = g

updateBoomhauer :: Ghost -> Board -> Ghost
updateBoomhauer g board = g

updateBill :: Ghost -> Board -> Ghost
updateBill g board = g

{-
------------------------------------------------------------
DRAW FUNCTIONS
------------------------------------------------------------
-}

drawGhosts :: [Ghost] -> [Picture]
drawGhosts gs = [drawHank, drawDale, drawBoomhauer, drawBill] -- todo

drawHank :: Ghost -> Picture
drawHank g = g

drawDale :: Ghost -> Picture
drawDale g = g

drawBoomhauer :: Ghost -> Picture
drawBoomhauer g = g

drawBill :: Ghost -> Picture
drawBill g = g