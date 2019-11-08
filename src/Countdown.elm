module Main exposing (Model, Msg(..), init, main, subscriptions, update, view)

import Browser
import Html
import Task
import Time


main : Program () Model Msg
main =
    Browser.element
        { init = init
        , update = update
        , view = view
        , subscriptions = subscriptions
        }


type Msg
    = Tick Time.Posix
    | AdjustTimeZone Time.Zone
    | CurrentTime Time.Posix


type alias Model =
    { zone : Time.Zone
    , time : Time.Posix
    }


init : flags -> ( Model, Cmd Msg )
init _ =
    ( Model Time.utc (Time.millisToPosix 0)
    , Cmd.batch
        [ Task.perform AdjustTimeZone Time.here
        , Task.perform CurrentTime Time.now
        ]
    )


kickOff : Time.Posix
kickOff =
    Time.millisToPosix 1578154400000


subscriptions : Model -> Sub Msg
subscriptions _ =
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

        CurrentTime timeNow ->
            ( { model | time = timeNow }
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
    Html.text (String.fromInt totalWeek ++ " weeks" ++ " , " ++ String.fromInt subDayWeek ++ " Days" ++ " , " ++ String.fromInt subHourDay ++ " Hours" ++ " , " ++ String.fromInt subMinuteHour ++ " Minutes" ++ " and " ++ String.fromInt subMinuteSeconds ++ " seconds")
