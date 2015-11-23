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
  { mode = Firing
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


view : Model -> Element.Element
view model =
  case model.mode of
    Aiming -> firingView model.data
    Firing -> firingView model.data


chooseEngine : Mode -> Engine
chooseEngine mode =
  let
    tick input =
      case input of
        FPS dt ->
          TimeEvolution.rungeKutta laws dt

        MouseAt _ ->
          identity
          
    firingEngine =
      { init = identity
      , update = tick
      , transition = always Nothing
      }
  in
    case mode of
      Aiming -> firingEngine
      Firing -> firingEngine
              
      
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

      
firingView : Data -> Element.Element
firingView model =
  let
    size =
      Vec2.vec2 400 400

    resolution =
      Vec2.vec2 20 20

    point pos =
      Collage.rect 20 20
        |> Collage.filled (colorOf pos model.terrain)
        |> Collage.move (Vec2.toTuple pos)

    points =
      gridInit point size resolution
        |> Collage.group

    ball =
      Collage.circle 5
        |> Collage.filled Color.red
        |> Collage.move (Vec2.toTuple model.position)
  in
    Collage.collage 500 500 [ points, ball ]


gridInit : (Vec2 -> a) -> Vec2 -> Vec2 -> List a
gridInit transform size resolution =
  let
    (resX, resY) =
      Vec2.toTuple resolution

    (scrX, scrY) =
      Vec2.toTuple size
          
    toCoord index arrayLength screenLength =
      ((toFloat index) / arrayLength - 0.5) * screenLength
      
    item j i =
      Vec2.vec2 (toCoord i resX scrX) (toCoord j resY scrY)
        |> transform
  in
    (\j -> Array.initialize (round resX) (item j))
    |> Array.initialize (round resY)
    |> Array.map Array.toList
    |> Array.toList
    |> List.concat


colorOf : Vec2 -> Bicubic.Spline -> Color
colorOf v spline =
  Bicubic.valueAt (Vec2.toRecord v) spline
    |> (\f -> (7 - f) / 7)
    |> Color.grayscale
