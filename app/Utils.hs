module Utils where

import Brillo

sameCol :: Point -> Point -> Bool
sameCol (x1, _) (x2, _) = x1 == x2

sameRow :: Point -> Point -> Bool
sameRow (_, y1) (_, y2) = y1 == y2