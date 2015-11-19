import Graphics.Element exposing (..)
import Graphics.Collage exposing (..)
import Mouse

import Queue exposing (Queue)


main : Signal Element
main =
  Signal.foldp update init inputs
  |> Signal.map view


type alias Vector =
  { x : Float, y : Float }

  
type alias Inputs =
  (Int, Int)
  

type alias Model =
  { head : Vector
  , tail : Vector
  , path : Queue Vector
  , minLength : Int
  }

                 
update : Inputs -> Model -> Model
update position model =
  let
    head =
      { x = toFloat (fst position - width//2)
      , y = toFloat (height//2 - snd position)
      }

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
  { head = { x = 0, y = 0 }
  , tail = { x = 0, y = 0 }
  , path = Queue.empty
  , minLength = 10
  }


inputs : Signal Inputs
inputs = Mouse.position


view : Model -> Element
view model =
  collage width height [ mouseline model ]

         
mouseline : Model -> Form
mouseline model =
  let
    point { x, y } =
      (x, y)
  in
    traced defaultLine (segment (point model.tail) (point model.head))


unitVector : Vector -> Vector
unitVector { x, y } =
  let
    magnitude =
      sqrt (x^2 + y^2)
  in
    { x = x / magnitude
    , y = y / magnitude
    }


subtract : Vector -> Vector -> Vector
subtract u v =
  { x = u.x - v.x
  , y = u.y - v.y
  }


add : Vector -> Vector -> Vector
add u v =
  { x = u.x + v.x
  , y = u.y + v.y
  }


scale : Float -> Vector -> Vector
scale n v =
  { x = v.x * n
  , y = v.y * n
  }


width : Int
width = 500

        
height : Int
height = 500
         
