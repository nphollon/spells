module Engine (update) where

import Math.Vector2 as Vec2

import Types exposing (..)
import TimeEvolution
import Interpolate.Bicubic as Bicubic
import Geometry exposing (fromCursor)
import Collision2D


update : Update -> Model -> Model
update up model =
  let
    engine =
      chooseEngine model.mode

    data =
      engine.update up model.data

    transition =
      engine.transition data
  in
    case transition of
      Nothing ->
        { model | data <- data }

      Just mode ->
        { mode = mode
        , data = .init (chooseEngine mode) data
        }


chooseEngine : Mode -> Engine
chooseEngine mode =
  case mode of
    Ready -> readyEngine
    Aim -> aimEngine
    Fire -> fireEngine


readyEngine : Engine
readyEngine =
  { init data =
      { data | continue <- False
      }
           
  , update input data =
      case input of
        FPS _ ->
          data
          
        MouseAt cursor ->
          { data | cursor <- cursor }

        Click ->
          { data | continue <-
                     Collision2D.isInside
                                  (fromCursor data.cursor) data.launchHull
          }

  , transition data =
      if data.continue then Just Aim else Nothing
  }


aimEngine : Engine
aimEngine =
  { init data =
      { data | continue <- False
             , position <- fromCursor data.cursor
      }

  , update input data =
      case input of
        FPS _ ->
          data

        MouseAt cursor ->
          { data | cursor <- cursor }

        Click ->
          { data | continue <- True }

  , transition data =
      if data.continue then Just Fire else Nothing
  }
               

fireEngine : Engine
fireEngine =
  { init data =
      { data | continue <- False
             , momentum <-
                 Vec2.direction (fromCursor data.cursor) data.position
                   |> Vec2.scale 100
      }
           
  , update input data =
      case input of
        FPS dt ->
          TimeEvolution.rungeKutta laws dt data
            |> checkCollisions

        MouseAt cursor ->
          { data | cursor <- cursor }

        Click ->
          { data | continue <- True }

  , transition data =
      if data.continue then Just Ready else Nothing
  }


laws : TimeEvolution.Laws Data
laws =
  { add a b =
      { a | position <- Vec2.add a.position b.position
          , momentum <- Vec2.add a.momentum b.momentum
      }
    
  , scale f a =
      { a | position <- Vec2.scale f a.position
          , momentum <- Vec2.scale f a.momentum
      }

  , force model =
      let
        gradient =
          Bicubic.gradientAt (Vec2.toRecord model.position) model.terrain
            |> Vec2.fromRecord

        discr =
          1 / sqrt (1 + Vec2.lengthSquared gradient)

      in
        { model | position <-
                    Vec2.scale (discr / model.mass) model.momentum
                , momentum <-
                    Vec2.scale (model.mass * model.g * discr) gradient
        }
  }


checkCollisions : Data -> Data
checkCollisions data =
  let
    doesRemain token =
      Vec2.distance data.position token > 10
  in
    { data | tokens <-
             List.filter doesRemain data.tokens
    }
