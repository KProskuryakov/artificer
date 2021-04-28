module Main where

import Prelude

import Data.DateTime.Instant (unInstant)
import Data.Int (round)
import Data.Maybe (Maybe(..))
import Data.Time.Duration (Milliseconds(..))
import Effect (Effect)
import Effect.Console (error)
import Effect.Now (now)
import Graphics.Canvas (CanvasElement, Context2D)
import Graphics.Canvas as C
import Web.HTML (window)
import Web.HTML.Window (requestAnimationFrame)

main :: Effect Unit
main = do
  mCanvas <- C.getCanvasElementById "game-canvas"
  cur_now <- curTime
  case mCanvas of 
    Just canvas -> C.getContext2D canvas >>= \ctx -> gameLoop canvas ctx newGame cur_now
    Nothing -> error "Could not find canvas element with id 'game-canvas'."

curTime :: Effect Number
curTime = do
  Milliseconds cur_now <- map unInstant now
  pure cur_now
  
gameLoop :: CanvasElement -> Context2D -> Game -> Number -> Effect Unit
gameLoop canvas ctx g prev_now = do
  w <- window
  cur_now <- curTime
  let dt = cur_now - prev_now
  let ng = updateGame g ( dt / 1000.0 )
  draw canvas ctx ng dt
  _ <- requestAnimationFrame (gameLoop canvas ctx ng cur_now) w
  pure unit

type Game = { x :: Number, y :: Number, vx :: Number, vy :: Number, width :: Number, height :: Number }

newGame :: Game
newGame = { x: 50.0, y: 50.0, vx: 0.0, vy: 0.0, width: 30.0, height: 30.0 }

updateGame :: Game -> Number -> Game
updateGame g dt = g { y = g.y + vy, vy = vy }
  where
    vy = g.vy + dt * 10.0

draw :: CanvasElement -> Context2D -> Game -> Number -> Effect Unit
draw canvas ctx g dt = do
  let fps = append "FPS: " $ show $ round $ 1000.0 / dt
  width <- C.getCanvasWidth canvas
  height <- C.getCanvasHeight canvas
  C.withContext ctx $ do
    C.clearRect ctx { x: 0.0, y: 0.0, width, height }
    C.setFillStyle ctx "black"
    C.fillRect ctx { x: g.x, y: g.y, width: 50.0, height: 50.0 }
    C.strokeText ctx fps 10.0 10.0
