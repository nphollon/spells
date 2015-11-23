module Update where

import Time exposing (Time)
import Interpolate.Bicubic exposing (Spline)
import Math.Vector2 exposing (Vec2)


type Update =
  FPS Time | MouseAt (Int, Int)


type Mode =
  Aiming | Firing


type alias Data =
  { position : Vec2
  , momentum : Vec2
  , terrain : Spline
  , mass : Float
  , g : Float
  }


type alias Engine =
  { init : Data -> Data
  , update : Update -> Data -> Data
  , transition : Data -> Maybe Mode
  }
