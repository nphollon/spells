module View (view) where

import Array
import Graphics.Element as Element exposing (Element)
import Graphics.Collage as Collage
import Color
import Text

import Interpolate.Bicubic as Bicubic
import Collision2D
import Math.Vector2 as Vec2 exposing (Vec2)

import Data
import Types exposing (..)
import Menu

view : Model -> Element
view model =
  case model.mode of
    Start -> Menu.view model.data
    Ready -> readyView model.data
    Aim -> aimView model.data
    Fire -> fireView model.data


readyView : Data -> Element
readyView data =
  let
    cursor =
      if Data.canLaunch data
      then crosshair
      else stopSign
        
  in
    onGrid data.level
             [ launchZone data.level.launchZone
             , drawTokens data.remainingTokens
             , Collage.move (Vec2.toTuple (Data.cursorVec data)) cursor
             ]


aimView : Data -> Element
aimView data =
  onGrid data.level
           [ drawTokens data.remainingTokens
           , pointer
             |> Collage.rotate (Data.cursorBearing data)
             |> Collage.move (Vec2.toTuple data.position)
           ]

           
fireView : Data -> Element
fireView data =
  Element.flow Element.down
         [ (onGrid data.level
                   [ ball
                       |> Collage.move (Vec2.toTuple data.position)
                   , drawTokens data.remainingTokens
                   ]
           )
         , Element.leftAligned (Text.fromString ("Score: " ++ toString data.score))
         ]

onGrid : Level -> List Collage.Form -> Element
onGrid level items =
  let
    scr = Vec2.toRecord level.size
    width = round scr.x
    height = round scr.y
  in
    Element.flow Element.inward
             [ (Collage.collage width height items)
             , level.image
             ]
    

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
    |> Collage.filled (Color.rgba 255 255 100 0.25)
