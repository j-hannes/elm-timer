module Main exposing (..)

import Html exposing (Html, text, div, img, button, input)
import Html.Attributes exposing (value, class, placeholder)
import Html.Events exposing (onClick)
import Svg exposing (circle, line, svg)
import Svg.Attributes
    exposing
        ( viewBox
        , width
        , cx
        , cy
        , r
        , fill
        , x1
        , y1
        , x2
        , y2
        , stroke
        , strokeDasharray
        , strokeDashoffset
        , strokeWidth
        , transform
        )
import Time exposing (Time, millisecond, inMilliseconds)
import Task


---- CONFIG ----


ticksPerSecond : Float
ticksPerSecond =
    30



---- MODEL ----


type alias Timer =
    { started : Time
    , elapsed : Int
    }


type alias Model =
    { timer : Maybe Timer }


init : ( Model, Cmd Msg )
init =
    ( { timer = Nothing }, Cmd.none )



---- UPDATE ----


type Msg
    = StartTimer
    | StopTimer
    | SetStartTime Time
    | Tick Time


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        StartTimer ->
            ( model, Task.perform SetStartTime Time.now )

        StopTimer ->
            ( { model | timer = Nothing }, Cmd.none )

        SetStartTime time ->
            ( { model | timer = Just <| Timer time 0 }, Cmd.none )

        Tick time ->
            case model.timer of
                Just timer ->
                    ( { model | timer = Just (updateTimer timer time) }, Cmd.none )

                Nothing ->
                    ( model, Cmd.none )


updateTimer : Timer -> Time -> Timer
updateTimer timer time =
    let
        ms =
            inMilliseconds time - inMilliseconds timer.started

        elapsed =
            round <| ms * ticksPerSecond / 1000
    in
        { timer | elapsed = elapsed }



---- VIEW ----


view : Model -> Html Msg
view model =
    div []
        [ input [ class "time-input", value "25", placeholder "mm" ] []
        , input [ class "time-input", value "00", placeholder "ss" ] []
        , viewButton model
        , div [ class "time-chart" ] [ viewTime model.timer ]
        ]


viewButton : Model -> Html Msg
viewButton model =
    case model.timer of
        Nothing ->
            button [ onClick StartTimer ] [ text "Start" ]

        Just _ ->
            button [ onClick StopTimer ] [ text "Stop" ]


viewTime : Maybe Timer -> Html Msg
viewTime timer =
    case timer of
        Nothing ->
            viewPie 0

        Just { elapsed } ->
            viewPie ((toFloat elapsed) / (1 * 60 * ticksPerSecond))


viewPie : Float -> Html Msg
viewPie percentage =
    svg [ viewBox "0 0 100 100", width "300px" ]
        [ circle
            [ cx "50"
            , cy "50"
            , r "40"

            -- , fill "#0B79CE"
            , fill "white"
            , stroke "#0B79CE"
            ]
            []
        , circle
            [ cx "50"
            , cy "50"
            , r "20"

            -- , fill "#0B79CE"
            -- , stroke "white"
            , fill "white"
            , stroke "#0B79CE"
            , strokeWidth "40"
            , strokeDasharray ((toString (percentage * 126)) ++ " 126")
            , transform "rotate(-90 50 50)"
            ]
            []
        ]



---- SUBSCRIPTIONS ----


subscriptions : Model -> Sub Msg
subscriptions model =
    case model.timer of
        Nothing ->
            Sub.none

        _ ->
            Time.every (1000 * millisecond / ticksPerSecond) Tick



---- PROGRAM ----


main : Program Never Model Msg
main =
    Html.program
        { view = view
        , init = init
        , update = update
        , subscriptions = subscriptions
        }
