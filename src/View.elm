module View (view) where

import Array
import Graphics.Element as Element exposing (Element)
import Graphics.Collage as Collage
import Color

import Interpolate.Bicubic as Bicubic
import Collision2D
import Math.Vector2 as Vec2 exposing (Vec2)

import Types exposing (..)
import Geometry exposing (fromCursor)


view : Model -> Element
view model =
  case model.mode of
    Ready -> readyView model.data
    Aim -> aimView model.data
    Fire -> fireView model.data


readyView : Data -> Element
readyView data =
  let
    position =
      fromCursor data.cursor

    cursor =
      if Collision2D.isInside position data.launchZone
      then crosshair
      else stopSign
        
  in
    onGrid data.terrain
             [ launchZone
             , drawTokens data.tokens
             , Collage.move (Vec2.toTuple position) cursor
             ]


aimView : Data -> Element
aimView data =
  let
    direction =
      Vec2.sub (fromCursor data.cursor) data.position

    angle =
      atan2 (Vec2.getY direction) (Vec2.getX direction)
  in
    onGrid data.terrain
             [ drawTokens data.tokens
             , pointer
                 |> Collage.rotate angle
                 |> Collage.move (Vec2.toTuple data.position)
             ]

           
fireView : Data -> Element
fireView data =
  onGrid data.terrain
           [ ball
               |> Collage.move (Vec2.toTuple data.position)
           , drawTokens data.tokens
           ]


onGrid : Bicubic.Spline -> List Collage.Form -> Element
onGrid terrain items =
  let
    resX = 12
    resY = 12

    scrX = 400
    scrY = 400
      
    toCoord index arrayLength screenLength =
      ((toFloat index) / arrayLength - 0.5) * screenLength
      
    pos i j =
      Vec2.vec2 (toCoord i resX scrX) (toCoord j resY scrY)

    gradient pos =
      Color.radial (0,0) 0 (0,0) (1 * scrX / resX)
             [ (0, Color.hsla 0 0 (level pos terrain) 0.9)
             , (0.3, Color.hsla 0 0 (level pos terrain) 0.8)
             , (1, Color.hsla 0 0 (level pos terrain) 0)
             ]

    level pos spline =
      Bicubic.valueAt (Vec2.toRecord pos) spline
        |> (\f -> f / 7)

    point pos =
      Collage.rect (2 * scrX / resX) (2 * scrY / resY)
        |> Collage.gradient (gradient pos)
        |> Collage.move (Vec2.toTuple pos)

    flatten =
      Array.map Array.toList >> Array.toList >> List.concat

    initialize2D m n f =
      Array.initialize m (\i -> Array.initialize n (f i))
           
  in
    initialize2D (round resX) (round resY) (\i j -> point (pos i j))
    |> flatten
    |> flip List.append items
    |> Collage.collage 500 500
    

ball : Collage.Form
ball =
  Collage.circle 5 |> Collage.filled Color.red


pointer : Collage.Form
pointer =
  [ (0, 0), (-25, 3)
  , (-30, 0), (-25, -3)
  ]
  |> Collage.polygon
  |> Collage.filled Color.darkYellow
    

crosshair : Collage.Form
crosshair =
  [ (3, 0), (10, 10)
  , (0, 3), (-10, 10)
  , (-3, 0), (-10, -10)
  , (0, -3), (10, -10)
  ]
  |> Collage.polygon
  |> Collage.filled Color.yellow
            

drawTokens : List Vec2 -> Collage.Form
drawTokens =
  let
    drawToken position =
      [ (10, 0), (0, 10), (-10, 0), (0, -10) ]
        |> Collage.polygon
        |> Collage.filled Color.blue
        |> Collage.move (Vec2.toTuple position)
    in
      List.map drawToken >> Collage.group


stopSign : Collage.Form
stopSign =
  [ (7, 3), (3, 7)
  , (-3, 7), (-7, 3)
  , (-7, -3), (-3, -7)
  , (3, -7), (7, -3)
  ]
  |> Collage.polygon
  |> Collage.filled Color.darkRed


launchZone : Collage.Form
launchZone =
  [ (-250, 250), (-250, -250), (0, 0) ]
    |> Collage.polygon
    |> Collage.filled (Color.rgba 255 255 100 0.5)
