module Countdown exposing (Model, Msg, init, subscriptions, update, view)

import Browser
import Html
import Task
import Time


main : Program () Model Msg
main =
    Browser.element
        { init = always init
        , update = update
        , view = view
        , subscriptions = always subscriptions
        }


type Msg
    = Tick Time.Posix
    | AdjustTimeZone Time.Zone


type alias Model =
    { zone : Time.Zone
    , time : Time.Posix
    }


init : ( Model, Cmd Msg )
init =
    ( Model Time.utc (Time.millisToPosix 0)
    , Cmd.batch
        [ Task.perform AdjustTimeZone Time.here
        , Task.perform Tick Time.now
        ]
    )


kickOff : Time.Posix
kickOff =
    Time.millisToPosix 1578154400000


addStrings : { week : Int, day : Int, hour : Int, minute : Int, second : Int } -> String
addStrings { week, day, hour, minute, second } =
    String.concat
        [ String.fromInt week
        , " weeks, "
        , String.fromInt day
        , " days, "
        , String.fromInt hour
        , " hours, "
        , String.fromInt minute
        , " minutes and "
        , String.fromInt second
        , " seconds"
        ]


subscriptions : Sub Msg
subscriptions =
    Time.every 1000 Tick


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


view : Model -> Html.Html Msg
view model =
    let
        timeLeft =
            Time.posixToMillis kickOff - Time.posixToMillis model.time

        totalSeconds =
            timeLeft // 1000

        subMinuteSeconds =
            modBy 60 totalSeconds

        totalMinutes =
            totalSeconds // 60

        subMinuteHour =
            modBy 60 totalMinutes

        totalHour =
            totalMinutes // 60

        subHourDay =
            modBy 24 totalHour

        totalDay =
            totalHour // 24

        subDayWeek =
            modBy 7 totalDay

        totalWeek =
            totalDay // 7
    in
    Html.text (addStrings { week = totalWeek, day = subDayWeek, hour = subHourDay, minute = subMinuteHour, second = subMinuteSeconds })
