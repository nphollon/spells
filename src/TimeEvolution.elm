module TimeEvolution (Laws, rungeKutta) where

import Time exposing (Time)


type alias Laws model =
  { add : model -> model -> model
  , scale : Float -> model -> model
  , force : model -> model
  }

                      
rungeKutta : Laws a -> Time -> a -> a
rungeKutta { add, scale, force } dt phase =
  let
    integrate =
      Time.inSeconds >> scale
          
    a = force phase
    b = a |> integrate (0.5 * dt) |> add phase |> force
    c = b |> integrate (0.5 * dt) |> add phase |> force
    d = c |> integrate dt |> add phase |> force
  in
    add a d
      |> add (scale 2 b)
      |> add (scale 2 c)
      |> integrate (dt / 6)
      |> add phase
