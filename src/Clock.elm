module Clock exposing (main)

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


timeLeft model =
    Time.millisToPosix (1578148200 - Time.posixToMillis model.time)


backgroundColor : Model -> String
backgroundColor model =
    let
        sec =
            toFloat (secondInt model)

        secondR =
            if sec < 40 then
                if sec < 20 then
                    sec

                else
                    40 - sec

            else
                0

        secondG =
            if 20 <= sec && sec <= 59 then
                if sec < 40 then
                    sec - 20

                else
                    60 - sec

            else
                0

        secondB =
            if 40 <= sec then
                sec - 40

            else if sec < 20 then
                20 - sec

            else
                0
    in
    "rgb("
        ++ String.fromFloat (secondR * 12.75)
        ++ ","
        ++ String.fromFloat (secondG * 12.75)
        ++ ","
        ++ String.fromFloat (secondB * 12.75)
        ++ ")"


secondInt : Model -> Int
secondInt model =
    Time.toSecond model.zone (timeLeft model)


view : Model -> Html.Html Msg
view model =
    let
        week =
            String.fromInt <| Basics.floor (toFloat day / 7)

        day =
            Time.toDay model.zone (timeLeft model)

        hour =
            String.fromInt (Time.toHour model.zone (timeLeft model))

        minute =
            String.fromInt (Time.toMinute model.zone (timeLeft model))

        second =
            String.fromInt (Time.toSecond model.zone (timeLeft model))
    in
    Html.div []
        [ Html.div [ style "color" (backgroundColor model) ]
            [ Html.text "Time until kickoff ~! \n "
            ]
        , Html.div []
            [ Html.text <|
                week
                    ++ " weeks "
                    ++ String.fromInt day
                    ++ " days "
                    ++ hour
                    ++ " hours "
                    ++ minute
                    ++ " minutes "
                    ++ second
                    ++ " seconds"
            ]
        ]
