module Test.Polyline exposing (main)

import Polyline
import ElmTest exposing (..)

-- tests : Test
tests =
    suite "Hashing" [
        test "has length" <| assertEqual (Polyline.encode [[1,2],[3,4]] 6) "hi"
        , test "empty" <| assertEqual (Polyline.encode [] 6) ""
        ]


main =
    runSuite tests
