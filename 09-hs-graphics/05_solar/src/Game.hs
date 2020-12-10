{-# LANGUAGE NamedFieldPuns #-}
module Game where

import qualified Board
import qualified Config
import Debug.Trace

import Graphics.Gloss.Game

data ItemState = ItemState { position  :: Board.Position
                           , speed     :: (Float, Float)
                           } deriving Show

data Mode = ModeSplash
          | ModeWon
          | ModeLost
          | ModeStart
          | ModeStill
          | ModeLeft
          | ModeRight
          | ModeUp
          | ModeDown
          deriving (Show, Eq)

data State = State { actorState   :: ItemState
                   , targetState  :: ItemState
                   , enemyState   :: ItemState
                   , mode         :: Mode
                   , windowSize   :: (Int, Int)
                   , contentScale :: Float
                   } deriving Show


initialState :: State
initialState = State { actorState   = initialActorState
                     , targetState  = initialTargetState
                     , enemyState   = initialEnemyState
                     , mode         = ModeSplash
                     , windowSize   = Config.windowSize
                     , contentScale = 1
                     }


-- | Respond to key events.
handleEvent :: Event -> State -> State

handleEvent (EventKey (SpecialKey KeyLeft) Down _ _) state  = state { mode = ModeLeft }
handleEvent (EventKey (SpecialKey KeyDown) Down _ _) state  = state { mode = ModeDown }
handleEvent (EventKey (SpecialKey KeyRight) Down _ _) state = state { mode = ModeRight }
handleEvent (EventKey (SpecialKey KeyUp) Down _ _) state    = state { mode = ModeUp }
handleEvent (EventKey (SpecialKey KeySpace) Down _ _) state = state { mode = ModeStart }

handleEvent (EventKey (Char '2') Down _ _) state = state { contentScale = 2.0 }
handleEvent (EventKey (Char '1') Down _ _) state = state { contentScale = 1.0 }

handleEvent (EventResize size) state = state { windowSize = size }

handleEvent _ state = state


itemsCollide state item1 item2 = let (x1, y1) = position $ item1 state
                                     (x2, y2) = position $ item2 state
                                     distance = sqrt ( (x1 - x2) ** 2 + (y1 - y2) ** 2 )
                                 in  distance < 0.5


update :: Float      -- ^ Broj sekundi od prethodnog azuriranja
       -> Game.State  -- ^ Predhodno stanje
       -> Game.State  -- ^ Novo stanje
update seconds oldState =
       let newState = oldState { Game.actorState  = actorUpdate seconds oldState
                               , Game.targetState = targetUpdate seconds oldState
                               , Game.enemyState  = enemyUpdate seconds oldState
                               }
       in if      itemsCollide newState Game.actorState Game.targetState then oldState { mode = ModeWon }
          else if itemsCollide newState Game.actorState Game.enemyState  then oldState { mode = ModeLost }
          else if itemsCollide newState Game.targetState Game.enemyState then oldState { mode = ModeLost }
          else    newState



isItemPositionValid position =
        let check cnr = Board.Hall == (Board.field $ Board.movedBy cnr position)
            offset    = 0.49 -- eps
        in  all check [ (offset, 0), (-offset, 0), (0, offset), (0, -offset) ]

data UpdateFunctions = UpdateFunctions { successMove  :: ItemState -> Board.Position
                                       , successSpeed :: ItemState -> (Float, Float)
                                       , failureMove  :: ItemState -> Board.Position
                                       , failureSpeed :: ItemState -> (Float, Float)
                                       }

generalItemMove :: (Game.State -> ItemState)
                -> UpdateFunctions
                -> Float
                -> Game.State
                -> (Bool, ItemState)
generalItemMove itemFunction UpdateFunctions { successMove, successSpeed, failureMove, failureSpeed } seconds oldWorld =
                let oldItem         = itemFunction oldWorld
                    oldPosition     = position oldItem
                    oldSpeed        = speed oldItem
                    desiredPosition = successMove oldItem
                in  if isItemPositionValid desiredPosition
                    then (True, ItemState { position = desiredPosition
                                          , speed    = successSpeed oldItem
                                          })
                    else (False, ItemState { position = failureMove oldItem
                                           , speed    = failureSpeed oldItem
                                           })


simpleItemMove :: String -> (Game.State -> ItemState) -> Float -> Game.State -> (Bool, ItemState)
simpleItemMove who itemFunction seconds oldWorld =
               let functions = UpdateFunctions { successMove  = \ item -> speed item + position item
                                               , successSpeed = \ item -> speed item
                                               , failureMove  = \ item -> position item
                                               , failureSpeed = \ item -> turnLeft $ speed item
                                               }
               in  generalItemMove itemFunction functions seconds oldWorld

turnLeft (0, dy) = if dy > 0 then (- dy, 0) else (- dy, 0)
turnLeft (dx, 0) = if dx > 0 then (0, dx) else (0, dx)

turnRight = turnLeft . turnLeft . turnLeft

-- Enemy

initialEnemyState = ItemState { position = Board.initialEnemyPosition
                              , speed    = (- 0.1, 0)
                              }


enemyUpdate :: Float -> Game.State -> ItemState
enemyUpdate seconds oldWorld = snd $ simpleItemMove "enemy" Game.enemyState seconds oldWorld


-- Target

initialTargetState = ItemState { position = Board.initialTargetPosition
                               , speed    = (0.1, 0)
                               }


targetUpdate :: Float -> Game.State -> ItemState
targetUpdate seconds oldWorld = snd $ simpleItemMove "target" Game.targetState seconds oldWorld


-- Actor

initialActorState = ItemState { position = Board.initialActorPosition
                              , speed    = (0, -0.1)
                              }


actorUpdate :: Float -> Game.State -> ItemState
actorUpdate seconds oldWorld =
               let newSpeed = case mode oldWorld of
                                   ModeLeft  -> (-0.1, 0)
                                   ModeRight -> (0.1, 0)
                                   ModeUp    -> (0, 0.1)
                                   ModeDown  -> (0, -0.1)
                                   _        -> (0, 0)
                   functions = UpdateFunctions { successMove  = \ item -> speed item + position item
                                               , successSpeed = \ item -> newSpeed
                                               , failureMove  = \ item -> position item
                                               , failureSpeed = \ item -> newSpeed
                                               }
               in  snd $ generalItemMove Game.actorState functions seconds oldWorld



