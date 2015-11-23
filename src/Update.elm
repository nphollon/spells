module Update where

import Time exposing (Time)
import Interpolate.Bicubic exposing (Spline, Vector)


type Update =
  FPS Time | MouseAt (Int, Int)


type Mode =
  Aiming | Firing


type alias Data =
  { position : Vector
  , momentum : Vector
  , terrain : Spline
  , mass : Float
  , g : Float
  }


type alias Engine =
  { init : Data -> Data
  , update : Update -> Data -> Data
  , transition : Data -> Maybe Mode
  }
