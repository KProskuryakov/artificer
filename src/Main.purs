module Main where

import Prelude
import Data.Array (foldl)
import Data.DateTime.Instant (unInstant)
import Data.Int (round)
import Data.Maybe (Maybe(..))
import Data.Time.Duration (Milliseconds(..))
import Data.Traversable (traverse)
import Effect (Effect)
import Effect.Console (error)
import Effect.Now (now)
import Effect.Ref (Ref)
import Effect.Ref as Ref
import Graphics.Canvas (CanvasElement, Context2D)
import Graphics.Canvas as C
import Main.Lines (Line, collide)
import Main.ControlEvents (ControlEvent(..), listenForControlEvents)
import Web.HTML (window)
import Web.HTML.Window (requestAnimationFrame)

canvasId :: String
canvasId = "game-canvas"

main :: Effect Unit
main = do
  events <- Ref.new ([] :: Array ControlEvent)
  listenForControlEvents events canvasId
  cur_now <- curTime
  mCanvas <- C.getCanvasElementById canvasId
  case mCanvas of
    Just canvas -> C.getContext2D canvas >>= \ctx -> gameLoop canvas ctx events newGame cur_now
    Nothing -> error "Could not find canvas element with id 'game-canvas'."

curTime :: Effect Number
curTime = do
  Milliseconds cur_now <- map unInstant now
  pure cur_now

gameLoop :: CanvasElement -> Context2D -> Ref (Array ControlEvent) -> Game -> Number -> Effect Unit
gameLoop canvas ctx events g prev_now = do
  w <- window
  cur_now <- curTime
  cur_events <- Ref.read events
  Ref.write [] events
  let
    eg = foldl processEvent g cur_events

    dt = cur_now - prev_now

    ug = updateGame eg (dt / 1000.0)
  draw canvas ctx ug dt
  _ <- requestAnimationFrame (gameLoop canvas ctx events ug cur_now) w
  pure unit

type Player
  = { x :: Number
    , y :: Number
    , vx :: Number
    , vy :: Number
    , width :: Number
    , height :: Number
    , grounded :: Boolean
    , sliding :: Boolean
    }

type Game
  = { p :: Player
    , lines :: Array Line
    }

newGame :: Game
newGame =
  { p:
      { x: 50.0
      , y: 50.0
      , vx: 0.0
      , vy: 0.0
      , width: 20.0
      , height: 20.0
      , grounded: false
      , sliding: false
      }
  , lines:
      [ { x1: 0.0
        , y1: 599.0
        , x2: 800.0
        , y2: 599.0
        }
      , { x1: 400.0
        , y1: 500.0
        , x2: 500.0
        , y2: 500.0
        }
      , { x1: 550.0
        , y1: 500.0
        , x2: 750.0
        , y2: 300.0
        }
      , { x1: 350.0
        , y1: 100.0
        , x2: 600.0
        , y2: 200.0
        }
      , { x1: 500.0
        , y1: 450.0
        , x2: 500.0
        , y2: 500.0
        }
      ]
  }

processEvent :: Game -> ControlEvent -> Game
-- processEvent g (Mouse1Up me) = g { p { x = me.x, y = me.y, vx = 0.0, vy = 0.0, grounded = false } }
processEvent g (Mouse1Up me) = g { p { x = me.x, y = me.y, vx = 0.0, vy = 0.0 } }

processEvent g (Mouse1Down _) = g

-- processEvent g JumpPress = if g.p.grounded == true then g { p { vy = -6.0, grounded = false } } else g
processEvent g JumpPress = g { p { vy = -6.0 } }

processEvent g JumpRelease = if g.p.vy < 0.0 then g { p { vy = 0.0 } } else g

processEvent g LeftPress = g { p { vx = -2.0 } }

processEvent g LeftRelease = if g.p.vx < 0.0 then g { p { vx = 0.0 } } else g

processEvent g RightPress = g { p { vx = 2.0 } }

processEvent g RightRelease = if g.p.vx > 0.0 then g { p { vx = 0.0 } } else g

processEvent g SlidePress = g { p { sliding = true } }

processEvent g SlideRelease = g { p { sliding = false } }

updateGame :: Game -> Number -> Game
updateGame g dt = g { p = cp }
  where
  bp = g.p { x = g.p.x + g.p.vx, vy = vy, y = y }

  vy = g.p.vy + dt * 30.0

  y = g.p.y + vy

  cp = foldl collide bp g.lines

draw :: CanvasElement -> Context2D -> Game -> Number -> Effect Unit
draw canvas ctx g dt = do
  let
    fps = append "FPS: " $ show $ round $ 1000.0 / dt
  width <- C.getCanvasWidth canvas
  height <- C.getCanvasHeight canvas
  C.withContext ctx
    $ do
        C.setFillStyle ctx "black"
        C.fillRect ctx { x: 0.0, y: 0.0, width, height }
        C.setFillStyle ctx "white"
        C.fillRect ctx { x: g.p.x - g.p.width / 2.0, y: g.p.y - g.p.height / 2.0, width: g.p.width, height: g.p.height }
        _ <- traverse (drawLine ctx) g.lines
        C.setStrokeStyle ctx "white"
        C.strokeText ctx fps 10.0 10.0

drawLine :: Context2D -> Line -> Effect Unit
drawLine ctx line = do
  C.withContext ctx
    $ do
        C.setStrokeStyle ctx "white"
        C.beginPath ctx
        C.moveTo ctx line.x1 line.y1
        C.lineTo ctx line.x2 line.y2
        C.closePath ctx
        C.stroke ctx
