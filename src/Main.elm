module Main exposing (Model, Msg(..), init, main, subscriptions, update, view)

import Browser
import Html exposing (..)
import Html.Attributes exposing (style)
import Task
import Time


main =
    Browser.element
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }


type alias Model =
    { zone : Time.Zone
    , time : Time.Posix
    }


fromStringToInt : String -> Int
fromStringToInt text =
    let
        theMaybe =
            String.toInt text
    in
    case theMaybe of
        Nothing ->
            0

        Just value ->
            value


init : () -> ( Model, Cmd Msg )
init _ =
    ( Model Time.utc (Time.millisToPosix 0)
    , Task.perform AdjustTimeZone Time.here
    )


type Msg
    = Tick Time.Posix
    | AdjustTimeZone Time.Zone


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Tick newTime ->
            ( { model | time = newTime }
            , Cmd.none
            )

        AdjustTimeZone newZone ->
            ( { model | zone = newZone }
            , Cmd.none
            )


subscriptions : Model -> Sub Msg
subscriptions model =
    Time.every 1000 Tick


view : Model -> Html Msg
view model =
    let
        hour =
            String.fromInt (Time.toHour model.zone model.time)

        minute =
            String.fromInt (Time.toMinute model.zone model.time)

        second =
            String.fromInt (Time.toSecond model.zone model.time)

        rgb =
            "rgb("
                ++ String.fromInt (255 - fromStringToInt hour * 4)
                ++ ","
                ++ String.fromInt (255 - fromStringToInt minute * 4)
                ++ ","
                ++ String.fromInt (255 - fromStringToInt second * 4)
                ++ ")"
    in
    h1 [ style "color" rgb ] [ text (hour ++ ":" ++ minute ++ ":" ++ second) ]
