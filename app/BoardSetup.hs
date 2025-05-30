module BoardSetup where

import Brillo
import Data
{-
  @author Roxanne Lutz
  setup of each level of the board. specify walls and special collectibles.
  very silly and hardcoded.
-}


{-
------------------------------------------------------------
LEVEL 1
------------------------------------------------------------
-}

lvl1Walls :: [Point]
lvl1Walls =  [
  -- general weirdness
  (-375, -375), (-375, -325), (-375, -275),(-375, -225),(-375, -125),(-375, -75),(-375, -25),
  (-375, 25), (-325, -275),(-325, -75), (-275, -375), (-275, -325), (-275, -275),(-275, -225), 
  (-275, -125),(-275, -75),(-275, -25),(-275, 25), (-225, -275),(-225, -75), (-175, -375), 
  (-175, -325), (-175, -275),(-175, -225), (-175, -125),(-175, -75),(-175, -25),(-175, 25),(175, -375), 
  (175, -325), (175, -275),(175, -225),(175, -125),(175, -75),(175, -25),(175, 25), (225, -275),(225, -75),
  (275, -375), (275, -325), (275, -275), (275, -225), (275, -125),(275, -75),(275, -25),(275, 25),(325, -275),(325, -75), 
  (375, -375), (375, -325), (375, -275),(375, -225), (375, -125),(375, -75),(375, -25),(375, 25),(-125, -375), (-125, -125),
  (-75, -75), (-75, -325),(-25, -325),(25, -25),(75, -275), (75, -25),(125, -225), (125, 25),
  -- big T
  (-375, 175),(-325, 175),(-275, 175),(-225, 175),(-175, 175),(-125, 175),(-75, 175),(-25, 175),
  (375, 175),(325, 175),(275, 175),(225, 175),(175, 175),(125, 175),(75, 175),(25, 175),
  (-375, 125),(375, 125),(-25, 125),(-25, 75),(25, 125),(25, 75)
  ]

lvl1SpecialCollectibles :: [Collectible]
lvl1SpecialCollectibles = [
  Collectible (GhostsOff 5.0) 5 red (275, -475),
  Collectible (GhostsOff 5.0) 5 red (-275, -475),
  Collectible (GhostsOff 5.0) 5 red (275, 225),
  Collectible (GhostsOff 5.0) 5 red (-275, 225),
  Collectible (GhostsOff 5.0) 5 red (275, -175),
  Collectible (GhostsOff 5.0) 5 red (-275, -175),
  Collectible (GhostsOff 5.0) 5 red (-75, 125),
  Collectible (GhostsOff 5.0) 5 red (75, 125)
  ]

{-
------------------------------------------------------------
LEVEL 2
------------------------------------------------------------
-}

lvl2Walls :: [Point]
lvl2Walls = [
  -- left squiggle
  (-425, 175),(-375, 175),
  (-375, 125),(-325, 125),
  (-325, 75),(-275, 75),
  (-275, 25),(-225, 25),
  (-225, -25),(-275, -25),

  (-175, -25),(-125, -25),
  (-175, -75),(-125, -75),
  (-175, -125),(-125, -125),

  (-275, -125),(-225, -125),
  (-325, -175),(-275, -175),
  (-375, -225),(-325, -225),
  (-425, -275),(-375, -275),

  -- middle left squiggle
  (-175, 175),(-125, 175),
  (-125, 125),(-75, 125),
  (-75, 75),(-25, 75),
  (-25, 25),(25, 25),
  (25, -25),(75, -25),

  (75, -25),(125, -25),
  (75, -75),(125, -75),
  (75, -125),(125, -125), 

  (75, -125), (25, -125),
  (25, -175),(-25, -175),
  (-25, -225),(-75, -225),
  (-75, -275),(-125, -275),
  (-125, -325), (-175, -325),
  (-175, -375), (-225, -375),
  (-225, -425), (-275, -425),

  -- right lower squiggle
  (75, -125), (125, -125),
  (125, -175),(175, -175),
  (175, -225),(225, -225),
  (225, -275),(275, -275),
  (275, -325), (325, -325),
  (325, -375), (375, -375),
  (375, -425), (425, -425),

  -- lines
  (125, 175),(175, 175),(225, 175),(275, 175),(325, 175),(375, 175),(425, 175),
  (175, 75),(225, 75),(275, 75),(325, 75),(375, 75),(425, 75),
  (225, -25),(275, -25),(325, -25),(375, -25),(425, -25),
  (275, -125),(325, -125),(375, -125),(425, -125),

  -- star for fun
  (75, -325), (75, -375), (75, -425),
  (125, -375), (25, -375)
  ]

lvl2SpecialCollectibles :: [Collectible]
lvl2SpecialCollectibles = [
  Collectible (GhostsOff 5.0) 5 red (25, -75),
  Collectible (GhostsOff 5.0) 5 red (-225, -75),
  Collectible (GhostsOff 5.0) 5 red (75, -175),
  Collectible (GhostsOff 5.0) 5 red (75, 25)
  ]

{-
------------------------------------------------------------
LEVEL 3
------------------------------------------------------------
-}

lvl3Walls :: [Point]
lvl3Walls = [
  (-425,175),(-375,175),(-325,175),(-275,175),(-225,175),(-175,175),(-125,175),           (-25,175),(25,175),       (125,175),(175,175),(225,175),(275,175),(325,175),(375,175),(425,175),
                                           (-225,125),                                                                             (225,125),
  (-425,75),(-375,75),(-325,75),(-275,75),(-225,75),(-175,75),(-125,75),(-75,75),(-25,75),(25,75),(75,75),(125,75),(175,75),(225,75),(275,75),(325,75),(375,75),(425,75),

  (-425,-25),(-375,-25),(-325,-25),(-275,-25),     (-175,-25),(-125,-25),(-75,-25),              (75,-25),(125,-25),(175,-25),        (275,-25),(325,-25),(375,-25),(425,-25),

  (-425,-125),(-375,-125),(-325,-125),(-275,-125),(-225,-125),(-175,-125),(-125,-125),(-75,-125),           (25,-125),(75,-125),(125,-125),(175,-125),(225,-125),(275,-125),(325,-125),(375,-125),(425,-125),
                                                  (-225,-175),                                                                                        (225,-175),
  (-425,-225),(-375,-225),(-325,-225),(-275,-225),(-225,-225),(-175,-225),            (-75,-225),(-25,-225),(25,-225),          (125,-225),(175,-225),(225,-225),(275,-225),(325,-225),(375,-225),(425,-225),
  
  (-425,-325),(-375,-325),(-325,-325),(-275,-325),(-225,-325),(-175,-325),(-125,-325),(-75,-325),                     (75,-325),(125,-325),(175,-325),(225,-325),(275,-325),(325,-325),(375,-325),(425,-325),
                                                  (-225,-375),                                                                                        (225,-375),
  (-425,-425),(-375,-425),(-325,-425),(-275,-425),(-225,-425),(-175,-425),            (-75,-425),(-25,-425),(25,-425),(75,-425),           (175,-425),(225,-425),(275,-425),(325,-425),(375,-425),(425,-425)
  ]

lvl3SpecialCollectibles :: [Collectible]
lvl3SpecialCollectibles = [
  Collectible (GhostsOff 5.0) 5 red  (-275, 125),
  Collectible (GhostsOff 5.0) 5 red  (-275, -175),
  Collectible (GhostsOff 5.0) 5 red  (-275,-375),
  Collectible (GhostsOff 5.0) 5 red  (275, 125),
  Collectible (GhostsOff 5.0) 5 red  (275, -175),
  Collectible (GhostsOff 5.0) 5 red  (275,-375),
  Collectible (GhostsOff 5.0) 5 red  (-225, 225),
  Collectible (GhostsOff 5.0) 5 red  (225, 225),
  Collectible (GhostsOff 5.0) 5 red  (-225, -475),
  Collectible (GhostsOff 5.0) 5 red  (225, -475)
  ]