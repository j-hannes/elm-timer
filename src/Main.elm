module Main exposing (..)

import Html exposing (Html, text, div, img, button, input)
import Html.Attributes exposing (value, class, placeholder, attribute)
import Html.Events exposing (onClick)


-- import Svg exposing (circle, line, svg)
-- import Svg.Attributes exposing (..)

import Time exposing (Time, millisecond, inMilliseconds, second)
import Task


---- CONFIG ----


ticksPerSecond : Float
ticksPerSecond =
    30


timerTimeInSeconds : Float
timerTimeInSeconds =
    25 * 60



---- MODEL ----


type alias Timer =
    { started : Time
    , elapsed : Int
    }


type PieState
    = None
    | Half
    | Full
    | Finished


type alias Model =
    { timer : Maybe Timer, pie : PieState }


init : ( Model, Cmd Msg )
init =
    ( { timer = Nothing, pie = None }, Cmd.none )



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
            ( { model | timer = Nothing, pie = None }, Cmd.none )

        SetStartTime time ->
            ( { model | timer = Just <| Timer time 0, pie = Half }, Cmd.none )

        Tick time ->
            case model.timer of
                Just timer ->
                    let
                        pie =
                            if (model.pie == Half) then
                                Full
                            else if (model.pie == Full) then
                                Finished
                            else if (model.pie == Finished) then
                                Finished
                            else
                                None
                    in
                        ( { model | timer = Just (updateTimer timer time), pie = pie }, Cmd.none )

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
        , div [ class "time-chart" ] [ viewTime model.timer model.pie ]
        ]


viewButton : Model -> Html Msg
viewButton model =
    case model.timer of
        Nothing ->
            button [ onClick StartTimer ] [ text "Start" ]

        Just _ ->
            button [ onClick StopTimer ] [ text "Stop" ]


viewTime : Maybe Timer -> PieState -> Html Msg
viewTime timer pie =
    case timer of
        Nothing ->
            viewPie 0 None

        Just { elapsed } ->
            let
                percentage =
                    (toFloat elapsed) / (1 * 60 * ticksPerSecond)
            in
                viewPie percentage pie


viewPie : Float -> PieState -> Html Msg
viewPie percentage pie =
    div
        [ class "pie"
        , class
            (if pie == Half then
                Debug.log "towards half" "toHalf"
             else if pie == Full then
                Debug.log "towards full" "toFull"
             else if pie == Finished then
                Debug.log "finished" "finished"
             else
                Debug.log "none" ""
            )
        , attribute "data-start" "100"
        , attribute "data-value" "260"
        ]
        []



---- SUBSCRIPTIONS ----


subscriptions : Model -> Sub Msg
subscriptions model =
    case model.timer of
        Nothing ->
            Sub.none

        _ ->
            Time.every (timerTimeInSeconds * second / 2) Tick



---- PROGRAM ----


main : Program Never Model Msg
main =
    Html.program
        { view = view
        , init = init
        , update = update
        , subscriptions = subscriptions
        }
