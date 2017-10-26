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
    10



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
                "one"
             else
                ""
            )

        -- , class
        --     (if percentage > 0 then
        --         "active"
        --      else
        --         ""
        --     )
        , attribute "data-start" "100"
        , attribute "data-value" "260"
        ]
        []



-- svg [ viewBox "0 0 100 100", width "300px" ]
--     [ circle
--         [ cx "50"
--         , cy "50"
--         , r "40"
--         , fill "white"
--         , stroke "#0B79CE"
--         ]
--         []
--     , circle
--         [ cx "50"
--         , cy "50"
--         , r "20"
--         , fill "white"
--         , stroke "#0B79CE"
--         , strokeWidth "40"
--         , strokeDasharray ((toString (percentage * 126)) ++ " 126")
--         , transform "rotate(-90 50 50)"
--         ]
--         []
--     ]
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
