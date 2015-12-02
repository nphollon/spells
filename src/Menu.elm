module Menu (engine, view) where

import Graphics.Element as Element exposing (Element)

import Math.Vector2 as Vec2
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
    bookHull =
      [ Vec2.vec2 0 140
      , Vec2.vec2 500 140
      , Vec2.vec2 500 360
      , Vec2.vec2 0 360
      ] |> Collision2D.fromVectors

    cursor =
      Vec2.vec2 (toFloat (fst data.cursor)) (toFloat (snd data.cursor))
                              
    imageSource =
      if Collision2D.isInside cursor bookHull then
        "/img/open_book.svg"
      else
        "/img/closed_book.svg"
  in
    Element.image 470 220 imageSource
      |> Element.container 500 500 Element.middle 
