module Contours (main) where

import Color
import Graphics.Collage as Collage
import Array

import Interpolate.Bicubic as Bicubic
import Math.Vector2 as Vec2

main =
  let
    resX = 1000
    resY = 500

    scr =
      { x = 1000, y = 500 }
      
    toCoord index arrayLength screenLength =
      ((toFloat index) / arrayLength - 0.5) * screenLength
      
    pos i j =
      Vec2.vec2 (toCoord i resX scr.x) (toCoord j resY scr.y)

    point pos =
      Collage.rect (2 * scr.x / resX) (2 * scr.y / resY)
        |> Collage.filled (color (value pos))
        |> Collage.move (Vec2.toTuple pos)

    flatten =
      Array.map Array.toList >> Array.toList >> List.concat

    initialize2D m n f =
      Array.initialize m (\i -> Array.initialize n (f i))
           
  in
    initialize2D (1 + round resX) (1 + round resY) (\i j -> point (pos i j))
    |> flatten
    |> Collage.collage (round scr.x) (round scr.y)


color a =
  Color.hsl
         (turns (0.6 - 0.3 * a))
         (1 - a)
         0.5

       
value pos =
  Bicubic.valueAt (Vec2.toRecord pos) terrain
    |> (\f -> (toFloat (round (2*f))) / 16)
       

terrain =
  Bicubic.rows level.knots
    |> Maybe.withDefault Bicubic.emptyData
    |> Bicubic.withRange
       (Vec2.toRecord (Vec2.scale -0.5 level.size))
       (Vec2.toRecord (Vec2.scale 0.5 level.size))

         
level =
  { knots =
      [ [ 7, 8, 9, 9, 9, 9, 9, 9, 9, 9 ]
      , [ 7, 6, 2, 3, 1, 0, 0, 0, 3, 9 ]
      , [ 7, 6, 0, 1, 0, 2, 4, 5, 1, 9 ]
      , [ 7, 6, 1, 3, 1, 2, 4, 5, 3, 9 ]
      , [ 7, 8, 9, 9, 9, 9, 9, 9, 9, 9 ]
      ]
    
  , size =
      Vec2.vec2 1000 500
  }
