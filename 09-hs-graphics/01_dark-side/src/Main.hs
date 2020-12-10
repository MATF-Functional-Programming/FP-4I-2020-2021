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

drawing :: Float -> Picture
-- drawing _ = circle 100
-- drawing _ = color white $ circle 100
-- drawing time = color white $ circle time
-- drawing _ = scale 200 200 $ color white $ circle 1
-- drawing _ = scale 200 200 $
--             color white $
--             polygon prismPath
drawing time = scale 200 200 $
          rotate time $
          pictures
              [ inRay
              , outRay
              , prismBackground
              , insideRay
              , prismBorder
              ]

inRay     = color white $ line [ (0, 0.14), (- prismSide, - prismHeight / 2.0) ]

outRay    = color black $ circle 0 -- Implementirati ovo za domaci

insideRay = color (greyN 0.5) $
            polygon [ (- prismSide / 4.0, 0)
                    , (  prismSide / 4.0, 0)
                    , (  prismSide * 0.30, -0.08)
                    ]

prismBackground = color (greyN 0.1) $ polygon prismPath
prismBorder     = color white $ lineLoop prismPath



main :: IO ()
main = animate window background drawing

