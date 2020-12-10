module Pictures where

import Graphics.Gloss.Data.Picture
import Graphics.Gloss.Game
import Graphics.Gloss

wall   = png "data/wall.png"
hall   = png "data/hall.png"

background = png "data/labyrinth-background.png"
splash     = png "data/labyrinth-splash.png"
lost       = png "data/labyrinth-lost.png"
won        = png "data/labyrinth-won.png"

itemPicture what = pictures [ color white $ rectangleWire 32 32, png ("data/" ++ what ++ "-001.png") ]

actor  = itemPicture "king"
target = itemPicture "baby"
enemy  = itemPicture "lcon"

