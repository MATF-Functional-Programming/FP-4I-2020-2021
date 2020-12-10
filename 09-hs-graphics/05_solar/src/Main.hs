--
-- Main.hs
-- Copyright (C) 2016 Ivan Čukić <ivan.cukic(at)kde.org>
--
-- Distributed under terms of the MIT license.
--

module Main where

import Graphics.Gloss
import Graphics.Gloss.Game

windowSize = 800

window :: Display
window = InWindow "Dark side" (800, 800) (10, 10)
         -- Alternativa je FullScreen

background :: Color
background = black

scaleEq factor = scale factor factor

sunPicture   = scaleEq (0.20 / 256.0) $ png "images/sun.png"
earthPicture = scaleEq (0.05 / 256.0) $ png "images/earth.png"
moonPicture  = scaleEq (0.03 / 256.0) $ png "images/moon.png"


view :: Float -> Picture
view time = scale (windowSize / 2) (windowSize / 2) $
     let hours           = 100 * time
         sunRotation     = 360.0 * hours / (15.0 * 24.0)

         earthRotation   = 360.0 * hours / (1.0 * 24.0)
         earthRevolution = 360.0 * hours / (365.0 * 24.0)

         moonRotation    = 360.0 * hours / (28.0 * 24.0)
         moonRevolution  = 360.0 * hours / (28.0 * 24.0)

     in pictures
              [ rotate sunRotation $ sunPicture
              , rotate earthRevolution $ translate 0.5 0 $
                    pictures [ rotate earthRotation earthPicture
                             , rotate moonRevolution $
                                     translate 0.1 0 $
                                 rotate moonRotation $ moonPicture
                             ]
              ]

main :: IO ()
main = animate window background view

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

