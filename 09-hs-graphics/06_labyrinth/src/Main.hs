module Main where

import Graphics.Gloss
import Graphics.Gloss.Game
import Graphics.Gloss.Data.ViewPort

import Debug.Trace

import Data.Maybe
import Data.List

import qualified Display as D
import qualified Board
import qualified Game
import qualified Config

render :: Game.State -> Picture
render state =
       let contentScale = Game.contentScale state
           content      = pictures [ D.board
                                   , D.actor  $ Game.actorState state
                                   , D.target $ Game.targetState state
                                   , D.enemy  $ Game.enemyState state
                                   ]
           splashScreen = D.splash $ Game.windowSize state
           wonScreen    = D.won    $ Game.windowSize state
           lostScreen   = D.lost   $ Game.windowSize state
       in  case Game.mode state of
                Game.ModeSplash -> splashScreen
                Game.ModeWon    -> wonScreen
                Game.ModeLost   -> lostScreen
                _ ->            pictures [ D.background $ Game.windowSize state
                                        , scale contentScale contentScale content
                                        ]

update_ :: ViewPort -> Float -> Game.State -> Game.State
update_ _ = Game.update

main :: IO ()
main = let size       = Config.windowSize
           position   = (600, 0)
           fps        = 30
           background = black
           window     = InWindow "Labyrinth" size position
           updates    = \ seconds state -> case Game.mode state of
                                               Game.ModeSplash  -> state
                                               Game.ModeStill   -> state
                                               _                -> Game.update seconds state
       in Graphics.Gloss.Game.play
              window
              background
              fps
              Game.initialState
              render
              Game.handleEvent
              [updates]

       -- in  simulate window color fps Game.initialState render update_

