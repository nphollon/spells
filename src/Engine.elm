module Engine (update) where

import Math.Vector2 as Vec2

import TimeEvolution
import Interpolate.Bicubic as Bicubic
import Collision2D

import Menu
import Data
import Types exposing (..)


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
        { model | data = data }

      Just mode ->
        { mode = mode
        , data = .init (chooseEngine mode) data
        }


chooseEngine : Mode -> Engine
chooseEngine mode =
  case mode of
    Start -> Menu.engine
    Ready -> readyEngine
    Aim -> aimEngine
    Fire -> fireEngine


readyEngine : Engine
readyEngine =
  { init = \data ->
      { data | continue = False
      }
           
  , update = \input data ->
      case input of
        FPS _ ->
          data
          
        MouseAt cursor ->
          { data | cursor = cursor }

        Click ->
          { data | continue = Data.canLaunch data }

  , transition = \data ->
      if data.continue then Just Aim else Nothing
  }


aimEngine : Engine
aimEngine =
  { init = \data ->
      { data | continue = False
             , position = Data.cursorVec data
      }

  , update = \input data ->
      case input of
        FPS _ ->
          data

        MouseAt cursor ->
          { data | cursor = cursor }

        Click ->
          { data | continue = True }

  , transition = \data ->
      if data.continue then Just Fire else Nothing
  }
               

fireEngine : Engine
fireEngine =
  { init = \data ->
      { data | continue = False
             , momentum =
                 Vec2.direction (Data.cursorVec data) data.position
                   |> Vec2.scale 100
      }
           
  , update = \input data ->
      case input of
        FPS dt ->
          TimeEvolution.evolve laws dt data
            |> checkCollisions

        MouseAt cursor ->
          { data | cursor = cursor }

        Click ->
          { data | continue = True }

  , transition = \data ->
      if data.continue then Just Ready else Nothing
  }


laws : TimeEvolution.Laws Data
laws =
  { add = \a b ->
      { a | position = Vec2.add a.position b.position
          , momentum = Vec2.add a.momentum b.momentum
      }
    
  , scale = \f a ->
      { a | position = Vec2.scale f a.position
          , momentum = Vec2.scale f a.momentum
      }

  , force = \model ->
      let
        gradient =
          Data.gradient model

        discr =
          1 / sqrt (1 + Vec2.lengthSquared gradient)

      in
        { model | position =
                    Vec2.scale (discr / model.mass) model.momentum
                , momentum =
                    Vec2.scale (model.mass * model.g * discr) gradient
        }
  }


checkCollisions : Data -> Data
checkCollisions data =
  let
    (remaining, collided) =
      List.partition
            (\token -> Vec2.distance data.position token > 13)
            data.remainingTokens

    score =
      data.score + List.length collided
  in
    { data | remainingTokens = remaining
           , score = score
    }
