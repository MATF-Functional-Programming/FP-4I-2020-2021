--
-- Main.hs
-- Copyright (C) 2016 Ivan Čukić <ivan.cukic(at)kde.org>
--
-- Distributed under terms of the MIT license.
--

module Main where

import Graphics.Gloss
import Graphics.Gloss.Game
import Graphics.Gloss.Data.ViewPort

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


update :: ViewPort -> Float -> Model -> Model
update _ time oldModel =
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


main :: IO ()
main = simulate
           window
           background
           120
           defaultModel
           (scale 400 400 . view)
           update

-- display  -- za crtanje staticke slike
-- display :: Display -> Color -> Picture -> IO ()
--
-- animate  -- za crtanje animacije koja zavisi od vremena
-- animate :: Display -> Color -> (Float -> Picture) -> IO ()
--
-- simulate -- za crtanje simulacije -- animacija sa modelom podataka
-- simulate :: Display     -- prozor
--          -> Color       -- boja pozadine
--          -> Int         -- broj slajdova po sekundi
--          -> model       -- pocetni model
--          -> (model -> Picture)
--                         -- funkcija prikaza
--          -> (ViewPort -> Float -> model -> model)
--                         -- funkcija azuriranja modela
--          -> IO ()
--
-- game -- simulacija + obrada dogadjaja
-- game :: Display     -- prozor
--      -> Color       -- boja pozadine
--      -> Int         -- broj slajdova po sekundi
--      -> world       -- pocetni svet
--      -> (world -> Picture)
--                     -- funkcija prikaza
--      -> (Event -> world -> world)
--                     -- funkcija azuriranja modela
--      -> (Float -> world -> world)
--                     -- funkcija azuriranja modela (vreme)
--      -> IO ()

