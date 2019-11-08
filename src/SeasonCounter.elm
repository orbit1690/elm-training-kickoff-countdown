module SeasonCounter exposing (..)

import Browser
import Html exposing (div, h2, text)
import Html.Attributes as Attributes exposing (style)
import Task exposing (Task)
import Time exposing (toHour, toMinute, toSecond, utc)


type alias Model =
    { current : Time.Posix
    , target : Time.Posix
    , zone : Time.Zone
    }


init : flags -> ( Model, Cmd Msg )
init _ =
    ( { current = Time.millisToPosix 0
      , zone = Time.utc
      , target = Time.millisToPosix 1578150000000
      }
    , Task.perform ChangeTimeZone Time.here
    )


type Msg
    = ChangeTime Time.Posix
    | ChangeTimeZone Time.Zone


view : Model -> Html.Html Msg
view model =
    let
        currentMillis : Int
        currentMillis =
            Time.posixToMillis model.current

        dateTimeMillis : Int
        dateTimeMillis =
            Time.posixToMillis model.target

        millisToKickOff : Int
        millisToKickOff =
            dateTimeMillis - currentMillis

        daysToKickOff : Int
        daysToKickOff =
            millisToKickOff // (60 ^ 2 * 24 * 1000)

        hoursToKickOff : Int
        hoursToKickOff =
            modBy (60 ^ 2 * 24 * 1000) millisToKickOff // (1000 * 60 ^ 2)

        minutesToKickOff : Int
        minutesToKickOff =
            modBy (1000 * 60 ^ 2) millisToKickOff // (1000 * 60)

        secondsToKickOff : Int
        secondsToKickOff =
            modBy (1000 * 60) millisToKickOff // 1000
    in
    div
        [ style "position" "absolute"
        , style "width" "90%"
        , style "height" "90%"
        ]
        [ Html.text (String.fromInt daysToKickOff ++ ":" ++ (String.fromInt hoursToKickOff ++ ":") ++ String.fromInt minutesToKickOff ++ ":" ++ String.fromInt secondsToKickOff) ]


update : Msg -> Model -> Model
update msg model =
    case msg of
        ChangeTime newTime ->
            { model | current = newTime }

        ChangeTimeZone newZone ->
            { model | zone = newZone }


subscriptions : Model -> Sub Msg
subscriptions always =
    Time.every 1000 ChangeTime


main : Program () Model Msg
main =
    Browser.element
        { init = init
        , view = view
        , update = \msg model -> ( update msg model, Cmd.none )
        , subscriptions = subscriptions
        }
