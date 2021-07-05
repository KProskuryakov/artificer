module Main.Lines where

import Prelude
import Data.Maybe (Maybe(..))
import Debug (trace)

-- import Data.Ord (abs)
type Point
  = { x :: Number
    , y :: Number
    }

type Line
  = { x1 :: Number
    , y1 :: Number
    , x2 :: Number
    , y2 :: Number
    }

type LineEquation
  = { a :: Number
    , b :: Number
    , c :: Number
    }

type Square r
  = { x :: Number
    , y :: Number
    , width :: Number
    , height :: Number
    , vx :: Number
    , vy :: Number
    , grounded :: Boolean
    , sliding :: Boolean
    | r
    }

toEquation :: Line -> LineEquation
toEquation l = { a: l.y1 - l.y2, b: l.x2 - l.x1, c: l.x2 * l.y1 - l.x1 * l.y2 }

getNormal :: LineEquation -> Point -> LineEquation
getNormal l p = { a: l.b, b: -l.a, c: l.b * p.x - l.a * p.y }

intersect :: LineEquation -> LineEquation -> Point
intersect l1 l2 = { x: (l1.c * l2.b - l1.b * l2.c) / det, y: (l1.a * l2.c - l1.c * l2.a) / det }
  where
  det = l1.a * l2.b - l1.b * l2.a

between :: forall a. Ord a => a -> a -> a -> Boolean
between a b x
  | a < b && x < a = false
  | a < b && x > b = false
  | b <= a && x < b = false
  | b <= a && x > a = false
  | true = true

lineBetween :: Line -> Point -> Boolean
lineBetween l p = between l.x1 l.x2 p.x && between l.y1 l.y2 p.y

distSq :: Point -> Point -> Number
distSq p1 p2 = a * a + b * b
  where
  a = p2.y - p1.y

  b = p2.x - p1.x

squareMaxDiameter :: forall r. Square r -> Number
squareMaxDiameter s = s.width * s.width + s.height * s.height

squareRadSq :: forall r. Square r -> Number
squareRadSq s = s.height * s.height / 4.0

centerOfMass :: forall r. Square r -> Point
centerOfMass s = { x: s.x, y: s.y }

nextSquare :: forall r. Square r -> Square r
nextSquare s = s { x = s.x + s.vx, y = s.y + s.vy }

intersectPoint :: forall r. Square r -> Line -> Maybe Point
intersectPoint s l = if lineBetween l int && distSq cen int <= squareRadSq s then Just int else Nothing
  where
  leq = toEquation l

  cen = centerOfMass $ nextSquare s

  n = getNormal leq cen

  int = intersect leq n

collide :: forall r. Square r -> Line -> Square r
collide p l = case intersectPoint p l of
  Just i -> p { y = i.y - p.height / 2.0, vy = 0.0 }
  Nothing -> p
