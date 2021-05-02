module Main where

import Prelude

import Data.Array (foldl)
import Data.Array as Arr
import Data.DateTime.Instant (unInstant)
import Data.Int (round, toNumber)
import Data.Maybe (Maybe(..), maybe)
import Data.Time.Duration (Milliseconds(..))
import Data.Traversable (traverse)
import Effect (Effect)
import Effect.Console (error)
import Effect.Now (now)
import Effect.Ref (Ref)
import Effect.Ref as Ref
import Graphics.Canvas (CanvasElement, Context2D)
import Graphics.Canvas as C
import Web.Event.Event (type_)
import Web.Event.EventTarget (addEventListener, eventListener)
import Web.Event.Internal.Types (Event)
import Web.HTML (window)
import Web.HTML.Window (requestAnimationFrame, toEventTarget)
import Web.UIEvent.KeyboardEvent (KeyboardEvent)
import Web.UIEvent.KeyboardEvent as KE
import Web.UIEvent.KeyboardEvent.EventTypes (keydown, keyup)
import Web.UIEvent.MouseEvent as ME
import Web.UIEvent.MouseEvent.EventTypes (mouseup)

main :: Effect Unit
main = do
  w <- window
  mCanvas <- C.getCanvasElementById "game-canvas"
  events <- Ref.new ([] :: Array Event)
  el <- eventListener (\e -> Ref.modify_ (\r -> Arr.snoc r e) events)
  addEventListener keydown el false (toEventTarget w)
  addEventListener keyup el false (toEventTarget w)
  addEventListener mouseup el false (toEventTarget w)
  cur_now <- curTime
  case mCanvas of
    Just canvas -> C.getContext2D canvas >>= \ctx -> gameLoop canvas ctx events newGame cur_now
    Nothing -> error "Could not find canvas element with id 'game-canvas'."

curTime :: Effect Number
curTime = do
  Milliseconds cur_now <- map unInstant now
  pure cur_now

gameLoop :: CanvasElement -> Context2D -> Ref (Array Event) -> Game -> Number -> Effect Unit
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

type Line
  = { x1 :: Number, y1 :: Number, x2 :: Number, y2 :: Number }

type Player
  = { x :: Number, y :: Number, vx :: Number, vy :: Number, width :: Number, height :: Number, grounded :: Boolean }

type Game
  = { p :: Player, lines :: Array Line }

newGame :: Game
newGame = { p: { x: 50.0, y: 50.0, vx: 0.0, vy: 0.0, width: 20.0, height: 20.0, grounded: false }, lines: [ { x1: 0.0, y1: 599.0, x2: 800.0, y2: 599.0 }, { x1: 400.0, y1: 500.0, x2: 500.0, y2: 500.0 } ] }

processEvent :: Game -> Event -> Game
processEvent g e
  | type_ e == mouseup = maybe g (\me -> g { p { x = toNumber $ ME.clientX me, y = toNumber $ ME.clientY me, vx = 0.0, vy = 0.0 } } ) ( ME.fromEvent e )
  | type_ e == keydown = maybe g ( keyDownEvent g ) ( KE.fromEvent e )
  | type_ e == keyup = maybe g ( keyUpEvent g ) ( KE.fromEvent e )
  | otherwise = g

keyDownEvent :: Game -> KeyboardEvent -> Game
keyDownEvent g e
  | KE.key e == "z" && g.p.grounded == true = g { p { vy = -6.0, grounded = false } }
  | KE.key e == "ArrowLeft" = g { p { vx = -4.0 } }
  | KE.key e == "ArrowRight" = g { p { vx = 4.0 } }
  | otherwise = g

keyUpEvent :: Game -> KeyboardEvent -> Game
keyUpEvent g e
  | KE.key e == "z" && g.p.vy < 0.0 = g { p { vy = 0.0 } }
  | KE.key e == "ArrowLeft" && g.p.vx < 0.0 = g { p { vx = 0.0 } }
  | KE.key e == "ArrowRight" && g.p.vx > 0.0 = g { p { vx = 0.0 } }
  | otherwise = g

updateGame :: Game -> Number -> Game
updateGame g dt = g { p { x = x, y = y, vy = vy, grounded = grounded } }
  where
  x = g.p.x + g.p.vx
  vy = if cp.grounded == false then g.p.vy + dt * 15.0 else 0.0
  cp = foldl collide g.p g.lines
  y = cp.y + vy
  grounded = cp.grounded

centerOfMass :: Player -> { cx :: Number, cy :: Number }
centerOfMass p = { cx: p.x + p.width / 2.0, cy: p.y + p.height / 2.0 }

collide :: Player -> Line -> Player
collide p l
  | between l.x1 l.x2 (centerOfMass p).cx || between l.y1 l.y2 (centerOfMass p).cy = if between p.y (p.y + p.height) l.y1 then p { y = l.y1 - p.height - 1.0, grounded = true } else p
  | otherwise = p

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
        C.fillRect ctx { x: g.p.x, y: g.p.y, width: g.p.width, height: g.p.height }
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
