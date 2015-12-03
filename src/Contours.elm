module Contours (main) where

import Color
import Graphics.Collage as Collage
import Array

import Interpolate.Bicubic as Bicubic
import Math.Vector2 as Vec2

import Levels
import Point


level =
  Levels.levelOne
        
main =
  let
    res =
      scr--      (250, 125)

    scr =
      (1000, 500)

    pixel =
      Point.map2 (\r s -> 2 * s / r) res scr
        |> uncurry Collage.rect
      
    toCoord index arrayLength screenLength =
      ((toFloat index) / arrayLength - 0.5) * screenLength
      
    positionOf index =
      Point.map3 toCoord index res scr

    drawPoint pos =
      Collage.filled (color (value pos)) pixel
        |> Collage.move pos

    flatten arrayArray =
      Array.map Array.toList arrayArray
        |> Array.toList
        |> List.concat

    grid (m, n) =
      Array.initialize m (\i -> Array.initialize n ((,) i))
           
  in
    Point.map ((+) 1) res
    |> grid
    |> flatten
    |> List.map (positionOf >> drawPoint)
    |> uncurry Collage.collage (Point.map round scr)


color a =
  Color.hsl
         (turns (0.6 - 0.3 * a))
         (1 - a)
         0.5

          
value pos =
  Bicubic.valueAt (Point.toRecord pos) terrain
    |> (\f -> (toFloat (round (2*f))) / 16)
       

terrain =
  Bicubic.rows level.knots
    |> Maybe.withDefault Bicubic.emptyData
    |> Bicubic.withRange
       (Vec2.toRecord (Vec2.scale -0.5 level.size))
       (Vec2.toRecord (Vec2.scale 0.5 level.size))
