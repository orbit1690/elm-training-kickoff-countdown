module Countdown exposing (Model, Msg(..), init, main, subscriptions, update, view)

import Browser
import Html exposing (..)
import Task
import Time



-- MAIN

main : Program () Model Msg
main =
    Browser.element
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }



-- MODEL


type alias Model =
    { zone : Time.Zone
    , time : Time.Posix
    }


init : () -> ( Model, Cmd Msg )
init _ =
    ( Model Time.utc (Time.millisToPosix 0)
    , Task.perform CurrTime Time.now
    )



-- UPDATE


type Msg
    = Tick Time.Posix
    | CurrTime Time.Posix


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
    




-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Time.every 1000 Tick

kickOff : Time.Posix
kickOff = 
    Time.millisToPosix 1578151800000

-- VIEW


view : Model -> Html Msg
view model =
    let
        timeleft = 
            Time.posixToMillis kickOff - Time.posixToMillis model.time

        totalSeconds =
            timeleft // 1000

        viewSeconds =
            String.fromInt(modBy 60 totalSeconds) ++ " Seconds"

        totalMinutes =
            totalSeconds // 60

        viewMinutes =
            String.fromInt(modBy 60 totalMinutes) ++ " Minutes"

        totalHours = 
            totalMinutes // 60

        viewHours =
            String.fromInt(modBy 24 totalHours) ++ " Hours"
            
        totalDays = 
            totalHours // 24

        viewDays =
            String.fromInt(totalDays) ++ " Days"
    in
    Html.div []
        [ h1 []
            [ text (viewDays ++ " : " ++viewHours ++ " : " ++ viewMinutes ++ " : " ++ viewSeconds) ]
        ]
