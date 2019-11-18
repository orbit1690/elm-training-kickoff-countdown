module ClockCountDown exposing (Model, Msg, init, subscriptions, update, view)

import Browser
import Html exposing (..)
import Task
import Time

main : Program () Model Msg 
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


type Msg
    = Tick Time.Posix
    | CurrTime Time.Posix


init : () -> ( Model, Cmd Msg )
init _=
    ( Model Time.utc (Time.millisToPosix 0)
    ,Task.perform CurrTime Time.now
    )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Tick newTime ->
            ( { model | time = newTime }
            , Cmd.none
            )

        CurrTime timeNow ->
            ( { model | time = timeNow }
            , Cmd.none
            )


subscriptions : Model -> Sub Msg
subscriptions model =
    Time.every 1000 Tick


kickOff : Time.Posix
kickOff =
    Time.millisToPosix 1578154400000


view : Model -> Html Msg
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
    Html.text (String.fromInt totalWeek ++ " weeks" ++ " , " ++ String.fromInt subDayWeek ++ " Days" ++ " , " ++ String.fromInt subHourDay ++ " Hours" ++ " , " ++ String.fromInt subMinuteHour ++ " Minutes" ++ " and " ++ String.fromInt subMinuteSeconds ++ " seconds")
