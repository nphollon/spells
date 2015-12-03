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
  { mode = Start
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
      [ [ 7, 8, 9, 9, 9, 9, 9, 9, 9, 9 ]
      , [ 7, 6, 2, 3, 1, 0, 0, 0, 3, 9 ]
      , [ 7, 6, 0, 1, 0, 2, 4, 5, 1, 9 ]
      , [ 7, 6, 1, 3, 1, 2, 4, 5, 3, 9 ]
      , [ 7, 8, 9, 9, 9, 9, 9, 9, 9, 9 ]
      ]
    
  , size = Vec2.vec2 1000 500
          
  , launchZone =
      [ Vec2.vec2 -500 250
      , Vec2.vec2 -500 -250
      , Vec2.vec2 -300 -250
      , Vec2.vec2 -300 250
      ]

  , tokens =
      [ Vec2.vec2 170 -190
      , Vec2.vec2 30 30
      ]
  }
