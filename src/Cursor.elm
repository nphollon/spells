module Cursor (main, cursor) where

import Graphics.Element exposing (..)
import Graphics.Collage exposing (..)
import Mouse

import Math.Vector2 as Vec2 exposing (Vec2)

import Queue exposing (Queue)


main : Signal Element
main =
  Signal.map view cursor


cursor : Signal Cursor
cursor =
  let
    toCursor model =
      { position = model.head
      , direction = Vec2.direction model.head model.tail
      }
  in
    Signal.foldp update init inputs
      |> Signal.map toCursor


type alias Inputs =
  (Int, Int)
  

type alias Model =
  { head : Vec2
  , tail : Vec2
  , path : Queue Vec2
  , minLength : Int
  }

                 
type alias Cursor =
  { position : Vec2
  , direction : Vec2
  }

                 
update : Inputs -> Model -> Model
update position model =
  let
    head =
      { x = toFloat (fst position - width//2)
      , y = toFloat (height//2 - snd position)
      } |> Vec2.fromRecord

    pathWithPush =
      Queue.push head model.path
    
    shouldUpdateTail =
      Queue.length pathWithPush > model.minLength

    (tail, path) =
      if | shouldUpdateTail ->
           Queue.pop pathWithPush
             |> Maybe.withDefault (model.tail, pathWithPush)
         | otherwise ->
           (model.tail, pathWithPush)
  in                  
    { model | head <- head
            , tail <- tail
            , path <- path
    }
  

init : Model
init =
  { head = Vec2.vec2 0 0
  , tail = Vec2.vec2 0 0
  , path = Queue.empty
  , minLength = 20
  }


inputs : Signal Inputs
inputs = Mouse.position


view : Cursor -> Element
view model =
  collage width height [ mouseline model ]

         
mouseline : Cursor -> Form
mouseline cursor =
  let
    tail =
      Vec2.add cursor.position
          (Vec2.scale -50 cursor.direction)
  in
    segment (Vec2.toTuple tail) (Vec2.toTuple cursor.position)
      |> traced defaultLine


width : Int
width = 500

        
height : Int
height = 500
         
