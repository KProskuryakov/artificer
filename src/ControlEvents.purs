module Main.ControlEvents where

import Prelude
import Data.Array as Arr
import Data.Int (toNumber)
import Data.Maybe (Maybe(..), maybe)
import Effect (Effect)
import Effect.Console (error)
import Effect.Ref (Ref)
import Effect.Ref as Ref
import Web.DOM.NonElementParentNode (getElementById)
import Web.Event.Event (type_)
import Web.Event.EventTarget (addEventListener, eventListener)
import Web.Event.Internal.Types (Event)
import Web.HTML (HTMLElement, window)
import Web.HTML.HTMLCanvasElement (fromElement, toEventTarget)
import Web.HTML.HTMLDocument (toNonElementParentNode)
import Web.HTML.HTMLElement (DOMRect, getBoundingClientRect)
import Web.HTML.HTMLElement as E
import Web.HTML.Window as W
import Web.UIEvent.KeyboardEvent (KeyboardEvent)
import Web.UIEvent.KeyboardEvent as KE
import Web.UIEvent.KeyboardEvent.EventTypes (keydown, keyup)
import Web.UIEvent.MouseEvent as ME
import Web.UIEvent.MouseEvent.EventTypes (mouseup)

listenForControlEvents :: Ref (Array ControlEvent) -> String -> Effect Unit
listenForControlEvents events canvasId = do
  w <- window
  d <- W.document w
  element <- getElementById canvasId (toNonElementParentNode d)
  let
    canvasTarget = element >>= fromElement >>= (toEventTarget >>> pure)

    htmlElement = element >>= E.fromElement
  case htmlElement of
    Just e -> do
      el <- eventListener (listener e)
      addEventListener keydown el false (W.toEventTarget w)
      addEventListener keyup el false (W.toEventTarget w)
      maybe (pure unit) (\c -> addEventListener mouseup el false c) canvasTarget
    Nothing -> error "Listeners failed to be added."
  where
  listener :: HTMLElement -> Event -> Effect Unit
  listener htmlElement event = do
    maybeControlEvent <- handleEvent htmlElement event
    maybe (pure unit) (\e -> Ref.modify_ (\r -> Arr.snoc r e) events) maybeControlEvent

  handleEvent :: HTMLElement -> Event -> Effect (Maybe ControlEvent)
  handleEvent htmlElement e
    | type_ e == mouseup && isLMB e = do
      bbox <- getBoundingClientRect htmlElement
      let
        mpos = getMouseCoords e
      pure $ Mouse1Up <$> (calcCanvasCoords <$> mpos <*> Just bbox)
    | type_ e == keydown = pure $ (KE.fromEvent e) >>= keyDownEvent
    | type_ e == keyup = pure $ (KE.fromEvent e) >>= keyUpEvent
    | otherwise = pure Nothing

  keyDownEvent :: KeyboardEvent -> Maybe ControlEvent
  keyDownEvent e
    | KE.key e == "z" = Just JumpPress
    | KE.key e == "ArrowLeft" = Just LeftPress
    | KE.key e == "ArrowRight" = Just RightPress
    | KE.key e == "LeftShift" = Just SlidePress
    | otherwise = Nothing

  keyUpEvent :: KeyboardEvent -> Maybe ControlEvent
  keyUpEvent e
    | KE.key e == "z" = Just JumpRelease
    | KE.key e == "ArrowLeft" = Just LeftRelease
    | KE.key e == "ArrowRight" = Just RightRelease
    | KE.key e == "LeftShift" = Just SlideRelease
    | otherwise = Nothing

  isLMB :: Event -> Boolean
  isLMB e = maybe false (\me -> ME.button me == 0) (ME.fromEvent e)

  getMouseCoords :: Event -> Maybe { x :: Number, y :: Number }
  getMouseCoords e = do
    me <- ME.fromEvent e
    pure { x: toNumber $ ME.clientX me, y: toNumber $ ME.clientY me }

  calcCanvasCoords :: { x :: Number, y :: Number } -> DOMRect -> { x :: Number, y :: Number }
  calcCanvasCoords mpos bbox = { x: mpos.x - bbox.left, y: mpos.y - bbox.top }

data ControlEvent
  = JumpPress
  | JumpRelease
  | LeftPress
  | LeftRelease
  | RightPress
  | RightRelease
  | SlidePress
  | SlideRelease
  | Mouse1Down { x :: Number, y :: Number }
  | Mouse1Up { x :: Number, y :: Number }
