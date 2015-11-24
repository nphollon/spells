module Main where

import Array exposing (Array)
import Color exposing (Color)
import Graphics.Element as Element
import Graphics.Collage as Collage

import Time exposing (Time)
import Mouse

import Update exposing (..)
import Interpolate.Bicubic as Bicubic
import TimeEvolution
import Math.Vector2 as Vec2 exposing (Vec2)


type alias Model =
  { mode : Mode
  , data : Data
  }


main : Signal Element.Element
main =
  Signal.foldp update init inputs
    |> Signal.map view


inputs : Signal Update
inputs =
  let
    fps =
      Signal.filter
              (\dt -> dt < Time.second * 0.25)
              0
              (Time.fps 40)
  in
    Signal.mergeMany
          [ Signal.map FPS fps
          , Signal.map MouseAt Mouse.position
          , Signal.map (always Click) Mouse.clicks
          ]
          
               

init : Model
init =
  { mode = Ready
  , data = initData
  }

          
initData : Data
initData =
  let
    data =
      [ [ 5, 5, 5, 5, 5 ]
      , [ 5, 2, 2, 4, 5 ]
      , [ 5, 1, 0, 1, 5 ]
      , [ 5, 1, 1, 3, 5 ]
      , [ 5, 5, 5, 5, 5 ]
      ]
         |> Bicubic.rows
         |> Maybe.withDefault Bicubic.emptyData

    start =
      { x = -200, y = -200 }

    end =
      { x = 200, y = 200 }
  in
    { terrain = Bicubic.withRange start end data
    , mass = 1
    , g = -5000
    , position = Vec2.vec2 100 -100
    , momentum = Vec2.vec2 0 0
    , continue = False
    , cursor = (0, 0)
    , tokens = [ Vec2.vec2 170 -190, Vec2.vec2 30 30 ]
    }


update : Update -> Model -> Model
update up model =
  let
    engine =
      chooseEngine model.mode

    data =
      engine.update up model.data

    transition =
      engine.transition data
  in
    case transition of
      Nothing ->
        { model | data <- data }

      Just mode ->
        { mode = mode
        , data = .init (chooseEngine mode) data
        }


chooseEngine : Mode -> Engine
chooseEngine mode =
  case mode of
    Ready -> readyEngine
    Aim -> aimEngine
    Fire -> fireEngine


readyEngine : Engine
readyEngine =
  { init data =
      { data | continue <- False
      }
           
  , update input data =
      case input of
        FPS _ ->
          data
          
        MouseAt cursor ->
          { data | cursor <- cursor }

        Click ->
          { data | continue <- True }

  , transition data =
      if data.continue then Just Aim else Nothing
  }


aimEngine : Engine
aimEngine =
  { init data =
      { data | continue <- False
             , position <- fromCursor data.cursor
      }

  , update input data =
      case input of
        FPS _ ->
          data

        MouseAt cursor ->
          { data | cursor <- cursor }

        Click ->
          { data | continue <- True }

  , transition data =
      if data.continue then Just Fire else Nothing
  }
               

fireEngine : Engine
fireEngine =
  { init data =
      { data | continue <- False
             , momentum <- momentum data
      }
           
  , update input data =
      case input of
        FPS dt ->
          TimeEvolution.rungeKutta laws dt data

        MouseAt cursor ->
          { data | cursor <- cursor }

        Click ->
          { data | continue <- True }

  , transition data =
      if data.continue then Just Ready else Nothing
  }


laws : TimeEvolution.Laws Data
laws =
  { add a b =
      { a | position <- Vec2.add a.position b.position
          , momentum <- Vec2.add a.momentum b.momentum
      }
    
  , scale f a =
      { a | position <- Vec2.scale f a.position
          , momentum <- Vec2.scale f a.momentum
      }

  , force model =
      let
        gradient =
          Bicubic.gradientAt (Vec2.toRecord model.position) model.terrain
            |> Vec2.fromRecord

        discr =
          1 / sqrt (1 + Vec2.lengthSquared gradient)

      in
        { model | position <-
                    Vec2.scale (discr / model.mass) model.momentum
                , momentum <-
                    Vec2.scale (model.mass * model.g * discr) gradient
        }
  }


momentum : Data -> Vec2
momentum data =
  Vec2.direction (fromCursor data.cursor) data.position
    |> Vec2.scale 100

       
view : Model -> Element.Element
view model =
  case model.mode of
    Ready -> readyView model.data
    Aim -> aimView model.data
    Fire -> fireView model.data


readyView : Data -> Element.Element
readyView data =
  let
    position =
      fromCursor data.cursor |> Vec2.toTuple
  in
    onGrid data.terrain
             [ drawTokens data.tokens
             , crosshair
                 |> Collage.move position
             ]


aimView : Data -> Element.Element
aimView data =
  let
    direction =
      momentum data

    angle =
      atan2 (Vec2.getY direction) (Vec2.getX direction)
  in
    onGrid data.terrain
             [ drawTokens data.tokens
             , pointer
                 |> Collage.rotate angle
                 |> Collage.move (Vec2.toTuple data.position)
             ]

           
fromCursor : (Int, Int) -> Vec2
fromCursor (x, y) =
  Vec2.vec2 (toFloat x - 250) (250 - toFloat y)

             
fireView : Data -> Element.Element
fireView data =
  onGrid data.terrain
           [ ball
               |> Collage.move (Vec2.toTuple data.position)
           , drawTokens data.tokens
           ]


onGrid : Bicubic.Spline -> List Collage.Form -> Element.Element
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
      Color.radial (0,0) 7 (0,0) (1 * scrX / resX)
             [ (0, Color.hsla 0 0 (level pos terrain) 1)
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
