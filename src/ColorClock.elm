module ColorClock exposing (main)

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
        , update = \msg model -> ( update msg model, Cmd.none )
        , subscriptions = subscriptions
        }


type alias Model =
    { time : Time.Posix
    , zone : Time.Zone
    }


init : flags -> ( Model, Cmd Msg )
init always =
    ( { time = Time.millisToPosix 0, zone = Time.utc }, Task.perform ChangeTimeZone Time.here )


type Msg
    = ChangeTime Time.Posix
    | ChangeTimeZone Time.Zone


view : Model -> Html.Html Msg
view model =
    let
        timeToString : (Time.Zone -> Time.Posix -> Int) -> String
        timeToString convertFunction =
            String.fromInt <| convertFunction model.zone model.time

        hour : String
        hour =
            timeToString Time.toHour

        minute : String
        minute =
            timeToString Time.toMinute

        second : String
        second =
            timeToString Time.toSecond
    in
    div
        [ style "position" "absolute"
        , style "width" "100%"
        , style "height" "100%"
        ]
        [ div
            [ style "background-color" <| String.concat [ "rgb(", hour, ", ", minute, ", ", second ]
            , style "width" "10%"
            , style "height" "10%"
            ]
            [ Html.text <| String.concat [ hour, " : ", minute, " : ", second ] ]
        ]


update : Msg -> Model -> Model
update msg model =
    case msg of
        ChangeTime newTime ->
            { model | time = newTime }

        ChangeTimeZone newZone ->
            { model | zone = newZone }


subscriptions : Model -> Sub Msg
subscriptions always =
    Time.every 1000 ChangeTime
