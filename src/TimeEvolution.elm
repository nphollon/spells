module TimeEvolution (Laws, Externals, externals, evolve) where

import List
import Signal exposing (Signal)
import Time exposing (Time)


type alias Laws model environment =
  { add : model -> model -> model
  , scale : Float -> model -> model
  , force : environment -> model -> model
  }


type alias Externals environment =
  (Time, environment)


externals : Time -> Signal Time -> Signal a -> Signal (Externals a)
externals limit fps env =
  let
    limited =
      Signal.filter (\delta -> delta < limit) 0 fps
  in
    Signal.map2 (,) limited env
      |> Signal.sampleOn limited

  
evolve : Laws a b -> a -> Signal (Externals b) -> Signal a
evolve config init extern =
  let
    transform =
      rungeKutta config
  in
    Signal.foldp transform init extern


rungeKutta : Laws a b -> Externals b -> a -> a
rungeKutta { add, scale, force } (dt, env) phase =
  let
    integrate =
      Time.inSeconds >> scale
          
    a = force env phase
    b = a |> integrate (0.5 * dt) |> add phase |> force env
    c = b |> integrate (0.5 * dt) |> add phase |> force env
    d = c |> integrate dt |> add phase |> force env
  in
    add a d
      |> add (scale 2 b)
      |> add (scale 2 c)
      |> integrate (dt / 6)
      |> add phase
