--
-- Main.hs
-- Copyright (C) 2016 Ivan Čukić <ivan.cukic(at)kde.org>
--
-- Distributed under terms of the MIT license.
--

module Main where

import Graphics.Gloss
import Graphics.Gloss.Game as Game
import Graphics.Gloss.Interface.Pure.Simulate

windowSize = 800

window :: Display
window = InWindow "Dark side" (800, 800) (10, 10)

background :: Color
background = white

blockSize = 0.05

data Model = Model
             { x  :: Float
             , y  :: Float
             , dx :: Float
             , dy :: Float
             }

defaultModel = Model 0 0 0.005 0.003

container :: Picture
container = color black $ rectangleWire 2 2


blockAt :: Float -> Float -> Picture
blockAt x y = translate x y $ color blue $ rectangleSolid blockSize blockSize


view :: Model -> Picture
view model = pictures [ container
                      , blockAt (x model) (y model)
                      ]


update :: Float -> Model -> Model
update time oldModel =
    let desired = oldModel { x  = x oldModel + dx oldModel
                           , y  = y oldModel + dy oldModel
                           , dy = dy oldModel - 0.0001
                           }

        xBumped = x desired + blockSize / 2 > 1 ||
                  x desired - blockSize / 2 < -1
        yBumped = y desired + blockSize / 2 > 1 ||
                  y desired - blockSize / 2 < -1

    in desired { dx = dx desired * if xBumped then (-1) else 1
               , dy = dy desired * if yBumped then (-1) else 1
               }


processEvent :: Event -> Model -> Model

processEvent (EventKey (SpecialKey KeySpace) Down _ (nx, ny)) model =
                      model { x = nx / 400
                            , y = ny / 400
                            }

processEvent (EventKey (SpecialKey KeyDown) Down _ _) model  = model { y = (-1) + blockSize / 2 }
processEvent (EventKey (SpecialKey KeyLeft) Down _ _) model  = model { x = (-1) + blockSize / 2 }
processEvent (EventKey (SpecialKey KeyRight) Down _ _) model = model { x = (1) - blockSize / 2 }




processEvent event world = world


main :: IO ()
main = Game.play
           window
           background
           120
           defaultModel
           (scale 400 400 . view)
           processEvent
           [ update ]

