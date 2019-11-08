module TimeColor exposing (main)

import Browser
import Html exposing (div, h2, text)
import Html.Attributes as Attributes exposing (style)
import Task exposing (Task)
import Time exposing (toHour, toMinute, toSecond, utc)


main : Program () Model Msg
main =
    Browser.element
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }


type alias Model =
    { time : Time.Posix
    , zone : Time.Zone
    }


init : flags -> ( Model, Cmd Msg )
init _ =
    ( { time = Time.millisToPosix 0, zone = Time.utc }, Task.perform ChangeTimeZone Time.here )


type Msg
    = ChangeTime Time.Posix
    | ChangeTimeZone Time.Zone


view : Model -> Html.Html Msg
view model =
    let
        hour : String
        hour =
            String.fromInt (Time.toHour model.zone model.time)

        minute : String
        minute =
            String.fromInt (Time.toMinute model.zone model.time)

        second : String
        second =
            String.fromInt (Time.toSecond model.zone model.time)

        _ =
            Debug.log "time" second
    in
    div
        [ style "position" "absolute"
        , style "width" "100%"
        , style "height" "100%"
        ]
        [ div
            [ style "background-color" ("rgb(" ++ hour ++ ", " ++ minute ++ ", " ++ second)
            , style "width" "10%"
            , style "height" "10%"
            ]
            [ Html.text (hour ++ " : " ++ minute ++ " : " ++ second) ]
        ]


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        ChangeTime newTime ->
            ( { model | time = newTime }
            , Cmd.none
            )

        ChangeTimeZone newZone ->
            ( { model | zone = newZone }
            , Cmd.none
            )


subscriptions : Model -> Sub Msg
subscriptions model =
    Time.every 1000 ChangeTime
