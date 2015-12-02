module Menu (engine, view) where

import Graphics.Element as Element exposing (Element)

import Math.Vector2 as Vec2 exposing (Vec2)
import Collision2D

import Types exposing (..)

engine : Engine
engine =
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
          { data | continue = True }
  , transition = \data ->
      if data.continue then Just Ready else Nothing
  }


view : Data -> Element
view data =
  let
    position (x, y) =
      Element.topLeftAt (Element.absolute x) (Element.absolute y)
             
    placeAt pt img =
      Element.container 1000 800 (position pt) img
  in
    [ bookIcon, handIcon ]
    |> List.map (drawIcon data.cursor)
    |> List.map2 placeAt [(10, 220), (450, 20)]
    |> Element.flow Element.outward


bookIcon : Icon
bookIcon =
  let
    hull =
      [ Vec2.vec2 0 0
      , Vec2.vec2 450 0
      , Vec2.vec2 450 800
      , Vec2.vec2 0 800
      ]
  in
    icon hull 470 220 "open_book.svg" "closed_book.svg"

  
handIcon : Icon
handIcon =
  let
    hull =
      [ Vec2.vec2 450 0
      , Vec2.vec2 800 0
      , Vec2.vec2 800 800
      , Vec2.vec2 450 800
      ]
  in
    icon hull 237 420 "open_hand.svg" "resting_fist.svg"

      
icon : List Vec2 -> Int -> Int -> String -> String -> Icon
icon hull width height focus blur =
  { hull = Collision2D.fromVectors hull
  , focusImage = image width height focus
  , blurImage = image width height blur
  }

  
drawIcon : (Int,Int) -> Icon -> Element
drawIcon (mouseX, mouseY) icon =
  let
    cursor =
      Vec2.vec2 (toFloat mouseX) (toFloat mouseY)
  in          
    if Collision2D.isInside cursor icon.hull then
      icon.focusImage
    else
      icon.blurImage


image : Int -> Int -> String -> Element
image width height filename =
  "/img/" ++ filename
    |> Element.image width height
       
         
type alias Icon =
  { hull : Collision2D.Hull
  , focusImage : Element
  , blurImage : Element
  }
