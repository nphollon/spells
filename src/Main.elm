module Main where

import Array exposing (Array)
import Color exposing (Color)
import Graphics.Element as Element
import Graphics.Collage as Collage
import Time exposing (Time)

import Interpolate.Bicubic as Bicubic
import TimeEvolution
import Math.Vector2 as Vec2 exposing (Vec2)

import Cursor exposing (Cursor)


main : Signal Element.Element
main =
  Signal.foldp update init inputs
    |> Signal.map view


update : Update -> Model -> Model
update input model =
  case input of
    FPS dt ->
      TimeEvolution.rungeKutta laws dt model

    MouseAt cursor ->
      model


inputs : Signal Update
inputs =
  let
    fps =
      Signal.filter
              (\dt -> dt < Time.second * 0.25)
              0
              (Time.fps 40)

    cursor =
      Cursor.cursor (400, 400)
  in
    Signal.merge
          (Signal.map FPS fps)
          (Signal.map MouseAt cursor)

          
type Update =
  FPS Time | MouseAt Cursor


type alias Model =
  { position : Vector
  , momentum : Vector
  , terrain : Bicubic.Spline
  , mass : Float
  , g : Float
  }
               
  
init : Model
init =
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
    , position = { x = 100, y = -100 }
    , momentum = { x = 0, y = 0 }
    }


laws : TimeEvolution.Laws Model
laws =
  { add a b =
      { a | position <- pointMap2 (+) a.position b.position
          , momentum <- pointMap2 (+) a.momentum b.momentum
      }
    
  , scale f a =
      { a | position <- pointMap ((*) f) a.position
          , momentum <- pointMap ((*) f) a.momentum
      }
    
  , force model =
      let
        gradient =
          Bicubic.gradientAt model.position model.terrain

        discr =
          sqrt (1 + gradient.x^2 + gradient.y^2)

        changePos p =
          p / model.mass / discr

        changeMom s =
          model.mass * model.g * s / discr
      in
        { model | position <- pointMap changePos model.momentum
                , momentum <- pointMap changeMom gradient
        }
  }

      
view : Model -> Element.Element
view model =
  let
    size =
      { x = 400, y = 400 }

    resolution =
      { x = 20, y = 20 }

    point pos =
      Collage.rect 20 20
        |> Collage.filled (colorOf pos model.terrain)
        |> Collage.move (pos.x, pos.y)

    points =
      gridInit point size resolution
        |> Collage.group

    ball =
      Collage.circle 5
        |> Collage.filled Color.red
        |> Collage.move (model.position.x, model.position.y)
  in
    Collage.collage 500 500 [ points, ball ]


gridInit : (Vector -> a) -> Vector -> Vector -> List a
gridInit f size resolution =
  let
    toCoords index =
      pointMap2 (\i n -> i/n - 0.5) index resolution
        |> pointMap2 (*) size
      
    item j i =
      f (toCoords { x = toFloat i, y = toFloat j })
  in
    (\j -> Array.initialize (round resolution.x) (item j))
    |> Array.initialize (round resolution.y)
    |> Array.map Array.toList
    |> Array.toList
    |> List.concat


colorOf : Vector -> Bicubic.Spline -> Color
colorOf v spline =
  Bicubic.valueAt v spline
    |> (\f -> (7 - f) / 7)
    |> Color.grayscale


pointMap : (a -> b) -> {x:a,y:a} -> {x:b,y:b}
pointMap f a =
  { x = f a.x
  , y = f a.y
  }

       
pointMap2 : (a -> b -> c) -> {x:a,y:a} -> {x:b,y:b} -> {x:c,y:c}
pointMap2 f a b =
  { x = f a.x b.x
  , y = f a.y b.y
  }

type alias Vector =
  Bicubic.Vector
