module Polyline exposing (encode)

{-| Polyline encoding

@docs polyline

-}

import Bitwise exposing (..)
import String
import Char
import List

encodeCoordinate : Float -> Float -> Float -> String

encodeCoordinate current previous factor =
  "hi"

{-| Transforms a list of coordinates into an encoded polyline
-}
encode : List a -> number -> String
encode coordinates precision =
  let
       concat [0, 0] coordinates
    factor = 10 ^ precision
    pair = List.take 2 coordinates
    a = List.head pair
    b = List.head (List.reverse pair)
  in
    case a of
      Nothing ->
        ""
      Just value ->
        "hi"

-- function(coordinates, precision) {
  --     if (!coordinates.length) { return ''; }
-- 
--   var factor = Math.pow(10, precision || 5),
--           output = encode(coordinates[0][0], 0, factor) + encode(coordinates[0][1], 0, factor);
-- 
--   for (var i = 1; i < coordinates.length; i++) {
--             var a = coordinates[i], b = coordinates[i - 1];
--                     output += encode(a[0], b[0], factor);
--                             output += encode(a[1], b[1], factor);
--                                 }
-- 
--   return output;
-- };

-- current = Math.round(current * factor);
-- previous = Math.round(previous * factor);
-- var coordinate = current - previous;
-- coordinate <<= 1;
-- if (current - previous < 0) {
--   coordinate = ~coordinate;
-- }
-- var output = '';
-- while (coordinate >= 0x20) {
--   output += String.fromCharCode((0x20 | (coordinate & 0x1f)) + 63);
--   coordinate >>= 5;
-- }
-- output += String.fromCharCode(coordinate + 63);

