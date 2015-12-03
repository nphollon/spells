module Point (map, map2, map3, toRecord) where

type alias Point a = (a, a)

map : (a -> b) -> Point a -> Point b
map f (x, y) =
  (f x, f y)
                  
map2 : (a -> b -> c) -> Point a -> Point b -> Point c
map2 f (x1, y1) (x2, y2) =
  (f x1 x2, f y1 y2)

map3 : (a -> b -> c -> d) -> Point a -> Point b -> Point c -> Point d
map3 f (x1, y1) (x2, y2) (x3, y3) =
  (f x1 x2 x3, f y1 y2 y3)

toRecord : Point a -> { x : a, y : a }
toRecord (x, y) =
  { x = x, y = y }
