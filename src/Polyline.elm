module Polyline exposing (encode)

{-| Polyline encoding

@docs polyline

-}

import Bitwise exposing (..)
import Maybe exposing (andThen)
import String
import Char
import List

encodeCoordinateShift : Int -> String -> String
encodeCoordinateShift coordinate output =
  if coordinate >= 0x20 then
    (encodeCoordinateShift (coordinate `shiftRight` 5)
      output ++ String.fromChar (Char.fromCode ((0x20 `or` (coordinate `and` 0x1f)) + 63)))
  else
    output ++ String.fromChar (Char.fromCode (coordinate + 63))

encodeCoordinate : Float -> Float -> Float -> String
encodeCoordinate current previous factor =
  let
    c = round(current * factor)
    p = round(previous * factor)
    shifted = (c - p) `shiftLeft` 1
    coordinate = if c - p < 0 then complement shifted else shifted
  in
    encodeCoordinateShift coordinate ""

encodeCoordinates : List (List number) -> String -> String
encodeCoordinates coords output =
  let
  in
    firstpair = List.take 2 coords
    a1 = List.head firstpair `andThen` List.head
    b1 = List.head firstpair `andThen` List.reverse `andThen` List.head

{-| Transforms a list of coordinates into an encoded polyline
-}
encode : List (List number) -> number -> String
encode coordinates precision =
  let
    coords = List.concat [[[0, 0]], coordinates]
    factor = 10 ^ precision
    -- here's where things get tricky: List (List.number) is our input
    -- and List.head returns a Maybe, and List.reverse requires a List,
    -- so it's hard to see how we'd get Just values from first, second
    -- and their values without nested case statements
    first = List.head coords
    second = List.head (List.tail coords)
  in
    encodeCoordinates coords ""
