module Main where

import Graphics.Element as Element
import Time exposing (Time)
import Mouse

import Math.Vector2 as Vec2 exposing (Vec2)
import Interpolate.Bicubic as Bicubic
import Collision2D

import Engine
import View
import Levels
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
  , data = initData Levels.levelOne
  }

          
initData : Level -> Data
initData level =
  { continue = False
               
  , mass = 1
  , g = -50000
  , position = Vec2.vec2 0 0
  , momentum = Vec2.vec2 0 0
  , cursor = (-100, -100)
  , score = 0
  , remainingTokens = level.tokens

  , level = level
  }
