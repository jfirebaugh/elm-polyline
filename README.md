# elm-polyline

A Google encoded polyline encoder in Elm. Soon to be a decoder as well

```elm
test "has length" <| assertEqual "_ibE_seK_seK_seK" (Polyline.encode [[1,2],[3,4]] 5)
```
