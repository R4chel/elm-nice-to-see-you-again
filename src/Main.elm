module Main exposing (..)

import Browser
import Browser.Events exposing (onAnimationFrame)
import Color exposing (Color)
import Html exposing (Html, button, div, text)
import Html.Events exposing (onClick)
import Random
import Svg exposing (..)
import Svg.Attributes exposing (..)



-- SHAPE


type alias Shape =
    { x : Int
    , y : Int
    , r : Int
    , color : Color
    , growing : Bool
    }



-- MODEL


type alias Model =
    { width : Int
    , height : Int
    , maxShapes : Int
    , minRadius : Int
    , maxRadius : Int
    , shapes : List Shape
    }



-- SHAPE FUNCTIONS


generateColor : Random.Generator Color
generateColor =
    Random.map4 Color.rgba
        (Random.float 0.0 1.0)
        (Random.float 0.0 1.0)
        (Random.float 0.0 1.0)
        (Random.float 0.0 1.0)


generateShape : Model -> Random.Generator Shape
generateShape model =
    Random.map5
        Shape
        (Random.int 0 model.width)
        (Random.int 0 model.height)
        (Random.constant model.minRadius)
        generateColor
        (Random.constant True)


viewShape : Shape -> Svg.Svg msg
viewShape shape =
    circle
        [ cx (String.fromInt shape.x)
        , cy (String.fromInt shape.y)
        , r (String.fromInt shape.r)
        , fill (Color.toCssString shape.color)
        , fillOpacity "0.30"
        ]
        []


collides : Shape -> Shape -> Bool
collides a b =
    (a.r + b.r) ^ 2 >= (a.x - b.x) ^ 2 + (a.y - b.y) ^ 2


shapeCanGrow : Model -> Shape -> (Shape -> Bool) -> Bool
shapeCanGrow model shape checkAgainstOtherShapes =
    shape.growing && shape.r <= model.maxRadius && shape.x - shape.r >= 0 && shape.x + shape.r <= model.width && shape.y - shape.r >= 0 && shape.y + shape.r <= model.height



-- && ( checkagainstOtherShapes shape )


growShapes : Model -> List Shape -> List Shape -> List Shape
growShapes model grownShapes toGrow =
    case toGrow of
        [] ->
            grownShapes

        hd :: tl ->
            let
                canGrow =
                    shapeCanGrow model hd (\shape -> not (List.any (collides shape) grownShapes || List.any (collides shape) tl))
            in
            let
                shape =
                    if not canGrow then
                        if hd.growing then
                            { hd | growing = False }

                        else
                            hd

                    else
                        { hd
                            | r = hd.r + 1
                        }
            in
            growShapes model (shape :: grownShapes) tl



-- MAIN


main =
    Browser.element { init = init, update = update, subscriptions = subscriptions, view = view }



-- INIT


init : () -> ( Model, Cmd Msg )
init () =
    let
        model =
            { width = 500, height = 500, maxShapes = 200, minRadius = 5, maxRadius = 50, shapes = [] }
    in
    ( model, Random.generate AddShape (generateShape model) )



-- UPDATE


type Msg
    = AddShape Shape
    | Grow
    | GenerateNewShape


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        AddShape shape ->
            ( { model | shapes = shape :: model.shapes }, Cmd.none )

        GenerateNewShape ->
            ( model, Random.generate AddShape (generateShape model) )

        Grow ->
            ( { model | shapes = growShapes model [] model.shapes }
            , Cmd.none
            )



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    if List.length model.shapes <= model.maxShapes then
        onAnimationFrame (\_ -> GenerateNewShape)

    else
        onAnimationFrame (\_ -> Grow)


view : Model -> Html Msg
view model =
    svg
        [ width (String.fromInt model.width)
        , height (String.fromInt model.height)
        , viewBox (String.join " " [ "0", "0", String.fromInt model.width, String.fromInt model.height ])
        ]
        (List.map
            viewShape
            model.shapes
        )
