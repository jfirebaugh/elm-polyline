module Test.Polyline exposing (main)

import Polyline
import ElmTest exposing (..)

-- tests : Test
tests =
    suite "Hashing" [
        test "has length" <| assertEqual "_ibE_seK_seK_seK" (Polyline.encode [[1,2],[3,4]] 5)
        , test "has length" <| assertEqual "_p~iF~ps|U_ulLnnqC_mqNvxq`@" (Polyline.encode [[38.5, -120.2], [40.7, -120.95], [43.252, -126.453]] 5)
        , test "empty" <| assertEqual "" (Polyline.encode [] 6)
        , test "encodeCoordinateShift" <| assertEqual "vB" (Polyline.encodeCoordinateShift 119)
        ]

main =
    runSuite tests
