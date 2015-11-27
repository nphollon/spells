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
  , data = initData levelOne
  }

          
initData : Level -> Data
initData level =
  let
    terrain =
      Bicubic.rows level.knots
        |> Maybe.withDefault Bicubic.emptyData
        |> Bicubic.withRange
           (Vec2.toRecord (Vec2.scale -0.5 level.size))
           (Vec2.toRecord (Vec2.scale 0.5 level.size))
  in
    { continue = False

    , mass = 1
    , g = -5000
    , position = Vec2.vec2 0 0
    , momentum = Vec2.vec2 0 0
    , cursor = (-100, -100)

    , size = level.size
    , terrain = terrain
    , tokens = level.tokens
    , launchHull = Collision2D.fromVectors level.launchZone
    , launchZone = level.launchZone
    }

  
levelOne : Level
levelOne =
  { knots =
      [ [ 5, 5, 5, 5, 5 ]
      , [ 5, 2, 2, 4, 5 ]
      , [ 5, 1, 0, 1, 5 ]
      , [ 5, 1, 1, 3, 5 ]
      , [ 5, 5, 5, 5, 5 ]
      ]
    
  , size = Vec2.vec2 400 400
          
  , launchZone =
      [ Vec2.vec2 180 -180
      , Vec2.vec2 180 -50
      , Vec2.vec2 80 -80
      , Vec2.vec2 50 -180
      ]

  , tokens =
      [ Vec2.vec2 170 -190
      , Vec2.vec2 30 30
      ]
  }
