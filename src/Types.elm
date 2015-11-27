module Types where

import Time exposing (Time)
import Interpolate.Bicubic exposing (Spline)
import Math.Vector2 as Vec2 exposing (Vec2)
import Collision2D exposing (Hull)


type alias Model =
  { mode : Mode
  , data : Data
  }


type Update =
  FPS Time | MouseAt (Int, Int) | Click


type Mode =
  Ready | Aim | Fire


type alias Data =
  { continue : Bool

  , mass : Float
  , g : Float

  , position : Vec2
  , momentum : Vec2
  , cursor : (Int, Int)
  , tokens : List Vec2

  , size : Vec2
  , terrain : Spline
  , launchHull : Hull
  , launchZone : List Vec2
  }

                
type alias Level =
  { knots : List (List Float)
  , size : Vec2
  , launchZone : List Vec2
  , tokens : List Vec2
  }


type alias Engine =
  { init : Data -> Data
  , update : Update -> Data -> Data
  , transition : Data -> Maybe Mode
  }


cursorVec : Data -> Vec2
cursorVec data =
  let
    (x, y) =
      data.cursor

    scr =
      Vec2.toRecord data.size
  in
    Vec2.vec2 (toFloat x - 0.5 * scr.x) (0.5 * scr.y - toFloat y)

