module Data where

import Math.Vector2 as Vec2 exposing (Vec2)
import Interpolate.Bicubic as Bicubic
import Collision2D

import Types exposing (..)

cursorVec : Data -> Vec2
cursorVec data =
  let
    (x, y) =
      data.cursor

    scr =
      Vec2.toRecord data.level.size
  in
    Vec2.vec2 (toFloat x - 0.5 * scr.x) (0.5 * scr.y - toFloat y)


canLaunch : Data -> Bool
canLaunch data =
  Collision2D.isInside (cursorVec data) data.level.launchHull


gradient : Data -> Vec2
gradient data =
  Bicubic.gradientAt (Vec2.toRecord data.position) data.level.terrain
    |> Vec2.fromRecord


cursorBearing : Data -> Float
cursorBearing data =
  let
    direction =
      Vec2.sub (cursorVec data) data.position
  in
    atan2 (Vec2.getY direction) (Vec2.getX direction)
