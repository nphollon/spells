module Types where

import Graphics.Element exposing (Element)
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
  Start | Ready | Aim | Fire


type alias Data =
  { continue : Bool

  , mass : Float
  , g : Float

  , position : Vec2
  , momentum : Vec2
  , cursor : (Int, Int)
  , score : Int
  , remainingTokens : List Vec2

  , level : Level
  }

                
type alias Level =
  { terrain : Spline
  , size : Vec2
  , launchHull : Hull
  , launchZone : List Vec2
  , tokens : List Vec2
  , image : Element
  }


type alias Engine =
  { init : Data -> Data
  , update : Update -> Data -> Data
  , transition : Data -> Maybe Mode
  }
