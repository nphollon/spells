module Main where

import Array exposing (Array)
import Color exposing (Color)
import Graphics.Element as Element
import Graphics.Collage as Collage

import Interpolate.Bicubic as Bicubic

main : Signal Element.Element
main = 
  Signal.foldp update init input |> Signal.map view


    
type alias Model =
  { terrain : Bicubic.Spline
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
    }



type Update =
  Dummy {}

           
input : Signal Update
input =
  Signal.map Dummy (Signal.constant {})



update : Update -> Model -> Model
update up model =
  case up of
    Dummy a ->
      model



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
  in
    Collage.collage 500 500 points


gridInit : (Vector -> a) -> Vector -> Vector -> List a
gridInit f size resolution =
  let
    toCoords index =
      pointMap (\i n -> i/n - 0.5) index resolution
        |> pointMap (*) size
      
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
        
pointMap : (a -> b -> c) -> {x:a,y:a} -> {x:b,y:b} -> {x:c,y:c}
pointMap f a b =
  { x = f a.x b.x
  , y = f a.y b.y
  }

type alias Vector =
  Bicubic.Vector
