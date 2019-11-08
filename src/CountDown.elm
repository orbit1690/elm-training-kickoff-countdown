module CountDown exposing (main)

import Browser
import Html
import Html.Attributes exposing (style)
import Maybe
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



{-
   digital clock
-}


type Msg
    = Tick Time.Posix
    | SetTimeZone Time.Zone Time.Posix


type alias Model =
    { time : Time.Posix
    , zone : Time.Zone
    }


kickOffMillis : Int
kickOffMillis =
    1578148200000


millisLeft : Model -> Int
millisLeft model =
    kickOffMillis - Time.posixToMillis model.time


subscriptions : Model -> Sub Msg
subscriptions _ =
    Time.every 1000 Tick


init : flags -> ( Model, Cmd Msg )
init _ =
    ( { time = Time.millisToPosix 0
      , zone = Time.utc
      }
    , Task.perform identity <| Task.map2 SetTimeZone Time.here Time.now
    )


update : Msg -> Model -> ( Model, Cmd msg )
update msg model =
    case msg of
        Tick timePosix ->
            ( { model | time = timePosix }
            , Cmd.none
            )

        SetTimeZone zone posix ->
            ( { model | time = posix, zone = zone }, Cmd.none )


backgroundColor : Model -> Int -> String
backgroundColor model sec =
    let
        partOfMinute : Int
        partOfMinute =
            sec // 20

        rgbCalc : Int -> Int -> Int
        rgbCalc up down =
            if partOfMinute == up then
                sec - (up * 20)

            else if partOfMinute == (down - 1) then
                (down * 20) - sec

            else
                0

        red : Float
        red =
            toFloat <| rgbCalc 0 2

        green : Float
        green =
            toFloat <| rgbCalc 1 3

        blue : Float
        blue =
            toFloat <| rgbCalc 2 1
    in
    "rgb("
        ++ String.fromFloat (red * 12.75)
        ++ ","
        ++ String.fromFloat (green * 12.75)
        ++ ","
        ++ String.fromFloat (blue * 12.75)
        ++ ")"


view : Model -> Html.Html Msg
view model =
    let
        millis : Int
        millis =
            modBy 1000 (millisLeft model)

        secondsLeft : Int
        secondsLeft =
            millisLeft model // 1000

        seconds : Int
        seconds =
            modBy 60 secondsLeft

        minutesLeft : Int
        minutesLeft =
            secondsLeft // 60

        minutes : Int
        minutes =
            modBy 60 minutesLeft

        hoursLeft : Int
        hoursLeft =
            minutesLeft // 60

        hours : Int
        hours =
            modBy 24 hoursLeft

        daysLeft : Int
        daysLeft =
            hoursLeft // 24

        days : Int
        days =
            modBy 7 daysLeft

        weeksLeft : Int
        weeksLeft =
            daysLeft // 7
    in
    Html.div []
        [ Html.div [ style "color" (backgroundColor model seconds) ]
            [ Html.text "Time until kickoff ~!"
            ]
        , Html.div []
            [ Html.text <|
                String.fromInt weeksLeft
                    ++ " weeks "
                    ++ String.fromInt days
                    ++ " days "
                    ++ String.fromInt hours
                    ++ " hours "
                    ++ String.fromInt minutes
                    ++ " minutes "
                    ++ String.fromInt seconds
                    ++ " seconds"
            ]
        ]
