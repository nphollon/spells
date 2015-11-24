module Geometry where

import Math.Vector2 as Vec2 exposing (Vec2)


fromCursor : (Int, Int) -> Vec2
fromCursor (x, y) =
  Vec2.vec2 (toFloat x - 250) (250 - toFloat y)
