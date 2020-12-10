--
-- Main.hs
-- Copyright (C) 2016 Ivan Čukić <ivan.cukic(at)kde.org>
--
-- Distributed under terms of the MIT license.
--

module Main where

import Graphics.Gloss

window :: Display
window = InWindow "Dark side" (400, 400) (10, 10)
         -- Alternativa je FullScreen

background :: Color
background = black

prismSide = 1.0
prismHeight = prismSide * sqrt(3.0) / 2.0
prismPath = [ (0.0, prismHeight / 2.0)
            , (- prismSide / 2.0, - prismHeight / 2.0)
            , (  prismSide / 2.0, - prismHeight / 2.0)
            ]

drawing :: Picture
drawing = scale 200 200 $
          pictures
              [ inRay
              , outRay
              , prismBackground
              , insideRay
              , prismBorder
              ]

(|>) = flip ($) -- flip id

inRay     = line [ (0, 0.14), (- prismSide, - prismHeight / 2.0) ]
            |> color white

outRay    = circle 0 -- Implementirati ovo za domaci
            |> color black

insideRay = polygon [ (- prismSide / 4.0, 0)
                    , (  prismSide / 4.0, 0)
                    , (  prismSide * 0.30, -0.08)
                    ]
            |> color (greyN 0.5)

prismBackground = polygon prismPath
                  |> color (greyN 0.1)
prismBorder     = lineLoop prismPath
                  |> color white



main :: IO ()
main = let display time = drawing |> rotate (5 * time)
       in animate window background display

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

