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


decode : String -> Int -> Float
decode str precision =
  decodeChange (decodeInner str precision 0 0 0)

decodeChange : Int -> Float
decodeChange result =
  if (result `and` 1) == 0 then
     Basics.toFloat (complement (result `shiftRight` 1))
  else
     Basics.toFloat (result `shiftRight` 1)

decodeInner : String -> Int -> Int -> Int -> Int -> Int
decodeInner str precision result shift byte =
  let
    factor = 10 ^ precision
    parts = String.uncons str
  in
     case parts of
       Nothing ->
         result
       Just (char, rest) ->
         if (Char.toCode char) - 63 >= 0x20 then
            result
         else
           (decodeInner
             rest
             precision 
             (result `or` ((Char.toCode char) - 63 `and` 0x1f) `shiftLeft` shift)
             (shift + 5)
             (Char.toCode char) - 63)
