module Main where

import Graphics.Element as Element
import Time exposing (Time)
import Mouse

import Math.Vector2 as Vec2 exposing (Vec2)
import Interpolate.Bicubic as Bicubic
import Collision2D

import Engine
import View
import Types exposing (..)


main : Signal Element.Element
main =
  Signal.foldp Engine.update init inputs
    |> Signal.map View.view


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

    launchZone =
      [ Vec2.vec2 180 -180
      , Vec2.vec2 180 -50
      , Vec2.vec2 80 -80
      , Vec2.vec2 50 -180
      ]
  in
    { terrain = Bicubic.withRange start end data
    , mass = 1
    , g = -5000
    , position = Vec2.vec2 100 -100
    , momentum = Vec2.vec2 0 0
    , continue = False
    , cursor = (0, 0)
    , tokens = [ Vec2.vec2 170 -190, Vec2.vec2 30 30 ]
    , launchHull = Collision2D.fromVectors launchZone
    , launchZone = launchZone
    }
