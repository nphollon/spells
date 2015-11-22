module Cursor (Cursor, main, cursor) where

import Graphics.Element exposing (..)
import Graphics.Collage exposing (..)
import Mouse

import Math.Vector2 as Vec2 exposing (Vec2)

import Queue exposing (Queue)


width : Int
width = 500

        
height : Int
height = 500
         

main : Signal Element
main =
  Signal.map view (cursor (width, height))


cursor : (Int, Int) -> Signal Cursor
cursor frameSize =
  let
    toCursor model =
      { position = model.head
      , direction = Vec2.direction model.head model.tail
      }

    origin =
      Vec2.scale 0.5 (toModelSpace frameSize)
  in
    Signal.foldp (update origin) init inputs
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

                 
update : Vec2 -> Inputs -> Model -> Model
update origin position model =
  let
    head =
      Vec2.sub (toModelSpace position) origin
        
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
  

toModelSpace : (Int, Int) -> Vec2
toModelSpace screen =
  Vec2.vec2
      (toFloat (fst screen))
      (toFloat (negate (snd screen)))
      
  
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
