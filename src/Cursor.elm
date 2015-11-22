module Cursor (Cursor, cursor) where

import Graphics.Element exposing (..)
import Graphics.Collage exposing (..)
import Mouse

import Queue exposing (Queue)


width : Int
width = 500

        
height : Int
height = 500
         

cursor : Point -> Signal Cursor
cursor frameSize =
  let
    toCursor model =
      { position = model.head
      , previous = model.tail
      }
  in
    Signal.foldp update init Mouse.position
      |> Signal.map toCursor


type alias Point =
  (Int, Int)  

  
type alias Model =
  { head : Point
  , tail : Point
  , path : Queue Point
  , minLength : Int
  }

                 
type alias Cursor =
  { position : Point
  , previous : Point
  }

                 
update : Point -> Model -> Model
update position model =
  let
    pathWithPush =
      Queue.push model.head model.path
    
    shouldUpdateTail =
      Queue.length pathWithPush > model.minLength

    (tail, path) =
      if | shouldUpdateTail ->
           Queue.pop pathWithPush
             |> Maybe.withDefault (model.tail, pathWithPush)
         | otherwise ->
           (model.tail, pathWithPush)
  in                  
    { model | head <- model.head
            , tail <- tail
            , path <- path
    }
  
  
init : Model
init =
  { head = (0, 0)
  , tail = (0, 0)
  , path = Queue.empty
  , minLength = 20
  }
