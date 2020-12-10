module Display ( showAt
               , splash
               , background
               , won
               , lost
               , board
               , actor
               , target
               , enemy
               ) where

import qualified Board
import qualified Game
import Config
import qualified Pictures as P

import Graphics.Gloss
import Graphics.Gloss.Game

showAt :: Board.Position -> Picture -> Picture
showAt (x, y) = translate (blockSize * x + boardOffsetHorizontal) (blockSize * y + boardOffsetVertical)

board :: Picture
board = showAt (0, 0) Board.picture

fullImage :: Picture -> (Int, Int) -> Picture
fullImage picture windowSize =
           let (_, (picWidth, picHeight)) = boundingBox picture
               (winWidth, winHeight)      = (fromIntegral $ fst windowSize, fromIntegral $ snd windowSize)
               horizontalScale            = winWidth / picWidth
               verticalScale              = winHeight / picHeight
               scaleFactor                = max horizontalScale verticalScale
           in scale scaleFactor scaleFactor $ picture

background windowSize = fullImage P.background windowSize
splash windowSize     = fullImage P.splash windowSize
won windowSize        = fullImage P.won windowSize
lost windowSize       = fullImage P.lost windowSize

actor  state = showAt (Game.position state) $ P.actor
target state = showAt (Game.position state) $ P.target
enemy  state = showAt (Game.position state) $ P.enemy

