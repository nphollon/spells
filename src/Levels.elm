module Levels where

import Math.Vector2 as Vec2

import Types exposing (..)

levelOne : Level
levelOne =
  { knots =
      [ [ 9, 9, 9, 9, 9, 9, 9, 9, 9, 9 ]
      , [ 9, 7, 6, 2, 3, 1, 0, 0, 3, 9 ]
      , [ 9, 7, 6, 0, 1, 0, 4, 5, 1, 9 ]
      , [ 9, 7, 6, 1, 3, 1, 4, 5, 3, 9 ]
      , [ 9, 9, 9, 9, 9, 9, 9, 9, 9, 9 ]
      ]
    
  , size = Vec2.vec2 1000 500
          
  , launchZone =
      [ Vec2.vec2 -310 175
      , Vec2.vec2 -380 150
      , Vec2.vec2 -420 110
      , Vec2.vec2 -420 -110
      , Vec2.vec2 -380 -150
      , Vec2.vec2 -310 -170
      , Vec2.vec2 -250 -100
      , Vec2.vec2 -250 100
      ]

  , tokens =
      [ Vec2.vec2 170 -190
      , Vec2.vec2 30 30
      ]

  , image = "/img/level_1.png"
  }
