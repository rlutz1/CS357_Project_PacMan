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
    go acc ((Ghost loc path curr next d u):gs) board = go ((u (Ghost loc path curr next d u) board) : acc) gs board

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
drawGhosts gs = go [] gs--foldr go [] gs --go gs board []
  where 
    go acc [] = acc
    go acc ((Ghost loc path curr next d u):gs) = go ((d (Ghost loc path curr next d u) ) : acc) gs 

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