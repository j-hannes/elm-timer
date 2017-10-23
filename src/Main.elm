module Main exposing (..)

import Html exposing (Html, text, div, img, button, input)
import Html.Attributes exposing (value, class, placeholder)
import Html.Events exposing (onClick)
import Time exposing (Time)
import Task
import Debug


---- MODEL ----


type alias Model =
    { time : Maybe Time }


init : ( Model, Cmd Msg )
init =
    ( { time = Nothing }, Cmd.none )



---- UPDATE ----


type Msg
    = StartTimer
    | NewTime Time


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    let
        loggedMsg =
            Debug.log "msg" msg

        loggedModel =
            Debug.log "model" model
    in
        case loggedMsg of
            StartTimer ->
                ( loggedModel, Task.perform NewTime Time.now )

            NewTime time ->
                ( { loggedModel | time = Just time }, Cmd.none )



---- VIEW ----


view : Model -> Html Msg
view model =
    div []
        [ input [ class "time-input", value "25", placeholder "mm" ] []
        , input [ class "time-input", value "00", placeholder "ss" ] []
        , button [ onClick StartTimer ] [ text "Start" ]
        , viewTime model
        ]


viewTime : Model -> Html Msg
viewTime model =
    div []
        [ text "time svg" ]



---- PROGRAM ----


main : Program Never Model Msg
main =
    Html.program
        { view = view
        , init = init
        , update = update
        , subscriptions = always Sub.none
        }
