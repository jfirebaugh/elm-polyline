module Polyline exposing (encode, encodeCoordinateShift, encodeCoordinate)

{-| Polyline encoding

@docs polyline

-}

import Bitwise exposing (..)
import Maybe exposing (andThen)
import String
import Char
import List

encodeCoordinateShift : Int -> String
encodeCoordinateShift coordinate =
  if coordinate >= 32 then
    (String.fromChar (Char.fromCode
        ((32 `or` (coordinate `and` 0x1f)) + 63)))
      ++ encodeCoordinateShift (coordinate `shiftRight` 5)
  else
    String.fromChar (Char.fromCode (coordinate + 63))

encodeCoordinate : Float -> Float -> Float -> String
encodeCoordinate factor previous current =
  if (Basics.isNaN current) then
    ""
  else
    let
      c = round(current * factor)
      p = round(previous * factor)
      shifted = (c - p) `shiftLeft` 1
      coordinate = if c - p < 0 then complement shifted else shifted
    in
      encodeCoordinateShift coordinate

encodeCoordinates : Float -> List Float -> List Float -> String
encodeCoordinates factor a b =
  List.foldr (++) "" (List.map2 (encodeCoordinate factor) a b)

{-| Transforms a list of coordinates into an encoded polyline
-}
encode : List (List Float) -> Float -> String
encode coordinates precision =
  if List.isEmpty coordinates then
    ""
  else
    List.foldr (++) "" (List.map2 (encodeCoordinates (10 ^ precision))
      (List.concat [[[0, 0]], coordinates])
      (List.concat [coordinates, [[0/0, 0/0]]]))
