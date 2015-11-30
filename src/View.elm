module View (view) where

import Array
import Graphics.Element as Element exposing (Element)
import Graphics.Collage as Collage
import Color

import Interpolate.Bicubic as Bicubic
import Collision2D
import Math.Vector2 as Vec2 exposing (Vec2)

import Types exposing (..)


view : Model -> Element
view model =
  case model.mode of
    Start -> startView model.data
    Ready -> readyView model.data
    Aim -> aimView model.data
    Fire -> fireView model.data


startView : Data -> Element
startView data =
  let
    bookHull =
      [ Vec2.vec2 0 140
      , Vec2.vec2 500 140
      , Vec2.vec2 500 360
      , Vec2.vec2 0 360
      ] |> Collision2D.fromVectors

    cursor =
      Vec2.vec2 (toFloat (fst data.cursor)) (toFloat (snd data.cursor))
                              
    imageSource =
      if Collision2D.isInside cursor bookHull then
        "/img/open_book.svg"
      else
        "/img/closed_book.svg"
  in
    Element.image 470 220 imageSource
      |> Element.container 500 500 Element.middle 


readyView : Data -> Element
readyView data =
  let
    position =
      cursorVec data
                
    cursor =
      if Collision2D.isInside position data.launchHull
      then crosshair
      else stopSign
        
  in
    onGrid data.size data.terrain
             [ launchZone data.launchZone
             , drawTokens data.tokens
             , Collage.move (Vec2.toTuple position) cursor
             ]


aimView : Data -> Element
aimView data =
  let
    direction =
      Vec2.sub (cursorVec data) data.position

    angle =
      atan2 (Vec2.getY direction) (Vec2.getX direction)
  in
    onGrid data.size data.terrain
             [ drawTokens data.tokens
             , pointer
                 |> Collage.rotate angle
                 |> Collage.move (Vec2.toTuple data.position)
             ]

           
fireView : Data -> Element
fireView data =
  onGrid data.size data.terrain
           [ ball
               |> Collage.move (Vec2.toTuple data.position)
           , drawTokens data.tokens
           ]


onGrid : Vec2 -> Bicubic.Spline -> List Collage.Form -> Element
onGrid size terrain items =
  let
    resX = 12
    resY = 12

    scr = Vec2.toRecord size
      
    toCoord index arrayLength screenLength =
      ((toFloat index) / arrayLength - 0.5) * screenLength
      
    pos i j =
      Vec2.vec2 (toCoord i resX scr.x) (toCoord j resY scr.y)

    gradient pos =
      Color.radial (0,0) 0 (0,0) (1 * scr.x / resX)
             [ (0, Color.hsla 0 0 (level pos terrain) 0.9)
             , (0.3, Color.hsla 0 0 (level pos terrain) 0.8)
             , (1, Color.hsla 0 0 (level pos terrain) 0)
             ]

    level pos spline =
      Bicubic.valueAt (Vec2.toRecord pos) spline
        |> (\f -> f / 7)

    point pos =
      Collage.rect (2 * scr.x / resX) (2 * scr.y / resY)
        |> Collage.gradient (gradient pos)
        |> Collage.move (Vec2.toTuple pos)

    flatten =
      Array.map Array.toList >> Array.toList >> List.concat

    initialize2D m n f =
      Array.initialize m (\i -> Array.initialize n (f i))
           
  in
    initialize2D (1 + round resX) (1 + round resY) (\i j -> point (pos i j))
    |> flatten
    |> flip List.append items
    |> Collage.collage (round scr.x) (round scr.y)
    

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
      Element.image 20 20 "/img/star.png"
        |> Collage.toForm
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


launchZone : List Vec2 -> Collage.Form
launchZone vertexes =
  List.map Vec2.toTuple vertexes
    |> Collage.polygon
    |> Collage.filled (Color.rgba 255 255 100 0.5)
