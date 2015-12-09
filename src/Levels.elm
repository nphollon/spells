module Levels (levelOne) where

import Math.Vector2 as Vec2 exposing (Vec2)
import Interpolate.Bicubic as Bicubic exposing (Spline)
import Collision2D
import Graphics.Element as Element

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
            
  } |> buildLevel


buildLevel : LevelParams -> Level
buildLevel params =
  { size =
      params.size
           
  , terrain =
      Bicubic.rows params.knots
        |> Maybe.withDefault Bicubic.emptyData
        |> Bicubic.withRange
           (Vec2.toRecord (Vec2.scale -0.5 params.size))
           (Vec2.toRecord (Vec2.scale 0.5 params.size))

  , tokens =
      params.tokens

  , launchHull =
      Collision2D.fromVectors params.launchZone

  , launchZone =
      params.launchZone

  , image =
      Element.image
               (round (Vec2.getX params.size))
               (round (Vec2.getY params.size))
               params.image
  }


type alias LevelParams =
  { knots : List (List Float)
  , size : Vec2
  , launchZone : List Vec2
  , tokens : List Vec2
  , image : String 
  }
