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
    Signal.merge
          (Signal.map FPS fps)
          (Signal.map MouseAt Mouse.position)
               

init : Model
init =
  { mode = Aiming
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
    , g = -1000
    , position = Vec2.vec2 100 -100
    , momentum = Vec2.vec2 0 0
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
    Aiming -> aimingEngine
    Firing -> firingEngine


aimingEngine : Engine
aimingEngine =
  { init = identity
           
  , update input data =
      case input of
        FPS _ ->
          data
          
        MouseAt (x, y) ->
          { data | position <- Vec2.vec2 (toFloat x - 250) (250 - toFloat y) }

  , transition = always Nothing
  }
               

firingEngine : Engine
firingEngine =
  { init = identity
           
  , update input =
      case input of
        FPS dt ->
          TimeEvolution.rungeKutta laws dt

        MouseAt _ ->
          identity

  , transition = always Nothing
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


view : Model -> Element.Element
view model =
  case model.mode of
    Aiming -> aimingView model.data
    Firing -> firingView model.data


aimingView : Data -> Element.Element
aimingView data =
  let
    crosshair =
      [ (3, 0), (10, 10)
      , (0, 3), (-10, 10)
      , (-3, 0), (-10, -10)
      , (0, -3), (10, -10)
      ]
         |> Collage.polygon
         |> Collage.filled Color.yellow
         |> Collage.move (Vec2.toTuple data.position)
  in
    onGrid data.terrain [ crosshair ]

             
firingView : Data -> Element.Element
firingView data =
  let
    ball =
      Collage.circle 5
        |> Collage.filled Color.red
        |> Collage.move (Vec2.toTuple data.position)
  in
    onGrid data.terrain [ ball ]


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
    
