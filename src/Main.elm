module Main exposing (..)

import Html exposing (Html, text, div, img, button, input)
import Html.Attributes exposing (value, class, placeholder)
import Html.Events exposing (onClick)
import Time exposing (Time, second, inSeconds)
import Task
import Debug


---- MODEL ----


type alias Timer =
    { started : Time
    , elapsed : Float
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
update smsg smodel =
    let
        msg =
            Debug.log "msg" smsg

        model =
            Debug.log "model" smodel
    in
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
    { timer | elapsed = inSeconds time - inSeconds timer.started }



---- VIEW ----


view : Model -> Html Msg
view model =
    div []
        [ input [ class "time-input", value "25", placeholder "mm" ] []
        , input [ class "time-input", value "00", placeholder "ss" ] []
        , viewButton model
        , viewTime model
        ]


viewButton : Model -> Html Msg
viewButton model =
    case model.timer of
        Nothing ->
            button [ onClick StartTimer ] [ text "Start" ]

        Just _ ->
            button [ onClick StopTimer ] [ text "Stop" ]


viewTime : Model -> Html Msg
viewTime model =
    div []
        [ text "time svg" ]



---- SUBSCRIPTIONS ----


subscriptions : Model -> Sub Msg
subscriptions model =
    case model.timer of
        Nothing ->
            Sub.none

        _ ->
            Time.every second Tick



---- PROGRAM ----


main : Program Never Model Msg
main =
    Html.program
        { view = view
        , init = init
        , update = update
        , subscriptions = subscriptions
        }
