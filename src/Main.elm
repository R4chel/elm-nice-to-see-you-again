module Main exposing (..)

import Html exposing (Html)
import Svg exposing (..)
import Svg.Attributes exposing (..)



-- Main


main : Html msg
main =
    svg
        [ viewBox "0 0 400 400"
        , width "400"
        , height "400"
        ]
        [ circle
            [ cx "50"
            , cy "50"
            , r "40"
            , fill "blue"
            , fillOpacity "0.30"
            ]
            []
        ]
