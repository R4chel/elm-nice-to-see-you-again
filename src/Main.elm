module Main exposing (..)

import Browser
import Browser.Events exposing (onAnimationFrame)
import Color exposing (Color)
import Html exposing (Html, button, div, text)
import Html.Events exposing (onClick)
import Random
import Svg exposing (..)
import Svg.Attributes exposing (..)



-- CONFIG


type alias Config =
    { width : Int
    , height : Int
    , maxShapes : Int
    , minRadius : Int
    , maxRadius : Int
    }



-- SHAPE


type alias Shape =
    { x : Int
    , y : Int
    , r : Int
    , color : Color
    , growing : Bool
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
        (Random.int 0 model.config.width)
        (Random.int 0 model.config.height)
        (Random.constant model.config.minRadius)
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


shapeCanGrow : Config -> Shape -> (Shape -> Bool) -> Bool
shapeCanGrow config shape checkAgainstOtherShapes =
    shape.growing && shape.r <= config.maxRadius && shape.x - shape.r >= 0 && shape.x + shape.r <= config.width && shape.y - shape.r >= 0 && shape.y + shape.r <= config.height && checkAgainstOtherShapes shape


growShapes : Config -> List Shape -> List Shape -> List Shape
growShapes config grownShapes toGrow =
    case toGrow of
        [] ->
            List.reverse grownShapes

        hd :: tl ->
            let
                canGrow =
                    shapeCanGrow config hd (\shape -> not (List.any (collides shape) grownShapes || List.any (collides shape) tl))
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
            growShapes config (shape :: grownShapes) tl



-- MODEL


type alias Model =
    { config : Config
    , shapes : List Shape
    }



-- MAIN


main =
    Browser.element { init = init, update = update, subscriptions = subscriptions, view = view }



-- INIT


init : () -> ( Model, Cmd Msg )
init () =
    let
        config =
            { width = 500, height = 500, maxShapes = 200, minRadius = 5, maxRadius = 50 }
    in
    let
        model =
            { config = config, shapes = [] }
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
            ( { model | shapes = growShapes model.config [] model.shapes }
            , Cmd.none
            )



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ if List.length model.shapes <= model.config.maxShapes then
            onAnimationFrame (\_ -> GenerateNewShape)

          else
            Sub.none
        , onAnimationFrame (\_ -> Grow)
        ]


view : Model -> Html Msg
view model =
    svg
        [ width (String.fromInt model.config.width)
        , height (String.fromInt model.config.height)
        , viewBox (String.join " " [ "0", "0", String.fromInt model.config.width, String.fromInt model.config.height ])
        ]
        (List.map
            viewShape
            model.shapes
        )
