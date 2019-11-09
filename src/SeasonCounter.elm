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
init always =
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

        secondsToKickOff : Int
        secondsToKickOff =
            millisToKickOff // 1000

        clockSeconds : String
        clockSeconds =
            String.fromInt <| modBy 60 secondsToKickOff

        minutesToKickOff : Int
        minutesToKickOff =
            secondsToKickOff // 60

        clockMinutes : String
        clockMinutes =
            String.fromInt <| modBy 60 minutesToKickOff

        hoursToKickOff : Int
        hoursToKickOff =
            minutesToKickOff // 60

        clockHours : String
        clockHours =
            String.fromInt <| modBy 24 minutesToKickOff

        daysToKickOff : Int
        daysToKickOff =
            hoursToKickOff // 24

        clockDays : String
        clockDays =
            String.fromInt <| modBy 365 daysToKickOff
    in
    div
        [ style "position" "absolute"
        , style "width" "90%"
        , style "height" "90%"
        ]
        [ Html.text <| String.concat <| [ clockDays, " : ", clockHours, " : ", clockMinutes ++ " : ", clockSeconds ] ]


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
