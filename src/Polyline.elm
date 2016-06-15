module Polyline exposing (encode, encodeCoordinateShift, encodeCoordinate)

{-| Polyline encoding

@docs polyline

-}

import Bitwise exposing (..)
import Maybe exposing (andThen)
import String
import Char
import List.Extra as List

stringFromCode : Int -> String
stringFromCode code = String.fromChar (Char.fromCode code)

encodeCoordinateShift : Int -> String
encodeCoordinateShift coordinate =
  if coordinate >= 32 then
    (stringFromCode ((32 `or` (coordinate `and` 0x1f)) + 63))
      ++ encodeCoordinateShift (coordinate `shiftRight` 5)
  else stringFromCode (coordinate + 63)

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
    List.foldr (++) "" (List.map (encodeCoordinates (10 ^ precision))
      (List.zip (List.concat [[[0, 0]], coordinates]) coordinates))


decode : String -> Int -> Maybe Float
decode str precision =
  case (decodeInner str precision 0 0 0) of
    Nothing -> Nothing
    Just (result, res) ->
      Just (decodeChange result)

decodeChange : Int -> Float
decodeChange result =
  if (result `and` 1) == 0 then
     Basics.toFloat (complement (result `shiftRight` 1))
  else
     Basics.toFloat (result `shiftRight` 1)

decodeInner : String -> Int -> Int -> Int -> Int -> Maybe (Int, String)
decodeInner str precision result shift byte =
  let
    factor = 10 ^ precision
    parts = String.uncons str
  in
     case parts of
       Nothing ->
         Nothing
       Just (char, rest) ->
         if (Char.toCode char) - 63 >= 0x20 then
           Just (result, rest)
         else
           (decodeInner
             rest
             precision 
             (result `or` ((Char.toCode char) - 63 `and` 0x1f) `shiftLeft` shift)
             (shift + 5)
             ((Char.toCode char) - 63))
