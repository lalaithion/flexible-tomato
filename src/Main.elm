module Main exposing (main)

-- TODO: I hate wildcard imports, remove them.

import Browser
import Bulma.CDN exposing (stylesheet)
import Bulma.Columns exposing (..)
import Bulma.Elements exposing (..)
import Bulma.Layout exposing (..)
import Bulma.Modifiers exposing (..)
import Bulma.Modifiers.Typography exposing (textCentered)
import Html exposing (Html, div, hr, main_, option, p, select, text)
import Html.Attributes exposing (attribute, class, disabled, selected, value)
import Html.Events exposing (onClick)
import List
import Maybe
import Time



-- TYPES


type Working
    = Working
    | Breaking
    | Paused


workingLabel : Working -> String
workingLabel w =
    case w of
        Working ->
            "🛠️ Start working"

        Breaking ->
            "🏖️ Take a break"

        Paused ->
            "🔒 (Pause the timer)"


workingColor : Working -> Color
workingColor w =
    case w of
        Working ->
            Danger

        Breaking ->
            Success

        Paused ->
            Info


type Pace
    = Langorious
    | Relaxed
    | Standard
    | Industrious
    | Grinding


ratio : Pace -> Float
ratio p =
    case p of
        Langorious ->
            1

        Relaxed ->
            1 / 2

        Standard ->
            1 / 3

        Industrious ->
            1 / 5

        Grinding ->
            1 / 11


description : Pace -> String
description p =
    case p of
        Langorious ->
            "Langorious (break for as long as you work)"

        Relaxed ->
            "Relaxed (spend 1/3 of your time on break)"

        Standard ->
            "Standard (spend 1/4 of your time on break)"

        Industrious ->
            "Industrious (spend 1/6 of your time on break)"

        Grinding ->
            "Grinding (spend 1/12 of your time on break)"


type Msg
    = Tick Time.Posix
    | Select Pace
    | ChangeState Working


type alias HistoryItem =
    { startTime : Maybe Time.Posix
    , endTime : Maybe Time.Posix
    , state : Working
    }


type alias Model =
    { savedTime {- In seconds -} : Float
    , lastTime : Maybe Time.Posix
    , startTime : Maybe Time.Posix
    , state : Working
    , pace : Pace
    , history : List HistoryItem
    }


initialModel : Model
initialModel =
    { savedTime = 0
    , lastTime = Nothing
    , startTime = Nothing
    , state = Paused
    , pace = Standard
    , history = []
    }



-- MAIN


main : Program () Model Msg
main =
    Browser.element
        { init = \_ -> ( initialModel, Cmd.none )
        , update = update
        , subscriptions = subscriptions
        , view = view
        }


subscriptions : Model -> Sub Msg
subscriptions model =
    Time.every 30 Tick



-- UPDATE


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    ( pureUpdate msg model, Cmd.none )


{-| pureUpdate exists because I was tired of writing Cmd.none everywhere, so I
wrapped all the logic in a pure variant of the update function. Unfortunately,
we need to send a notification inside of here, which I don't know how to do
yet...
-}
pureUpdate : Msg -> Model -> Model
pureUpdate msg model =
    case msg of
        Select p ->
            { model | pace = p }

        Tick currentTime ->
            case model.state of
                Working ->
                    updateTimes
                        (\oldSaved oldTime ->
                            oldSaved + timeDiff currentTime oldTime * ratio model.pace
                        )
                        currentTime
                        model

                Breaking ->
                    updateTimes
                        (\oldSaved oldTime ->
                            let
                                newSaved =
                                    oldSaved - timeDiff currentTime oldTime
                            in
                            if newSaved < 0 then
                                0
                                -- TODO: Send an alert here

                            else
                                newSaved
                        )
                        currentTime
                        model

                Paused ->
                    updateTimes (\oldSaved oldTime -> oldSaved) currentTime model

        ChangeState s ->
            updateState s model


updateState : Working -> Model -> Model
updateState s model =
    if model.state /= s then
        { model
            | state = s
            , startTime = model.lastTime
            , history =
                { state = model.state, endTime = model.lastTime, startTime = model.startTime }
                    :: model.history
        }

    else
        model


{-| updateTimes is a wrapper around handling the initial case in the model,
where lastTime is Nothing. Instead, you can always assume that it is Just
lastTime, and pass that behavior into here, which handles all of the
initialization logic.
-}
updateTimes : (Float -> Time.Posix -> Float) -> Time.Posix -> Model -> Model
updateTimes f currentTime model =
    case model.lastTime of
        Nothing ->
            { model | lastTime = Just currentTime }

        Just lastTime ->
            let
                newSaved =
                    f model.savedTime lastTime
            in
            { model | savedTime = newSaved, lastTime = Just currentTime }



-- VIEW


view : Model -> Html Msg
view model =
    -- TODO: Can this be broken up at all?
    main_ []
        [ stylesheet
        , level []
            [ levelItem [ textCentered ]
                [ select []
                    [ paceOption model Langorious
                    , paceOption model Relaxed
                    , paceOption model Standard
                    , paceOption model Industrious
                    , paceOption model Grinding
                    ]
                ]
            ]
        , level []
            [ levelItem [ textCentered ]
                [ div []
                    [ p [ class "header" ] [ text "Time saved up for your break" ]
                    , p [ class "title" ] [ text (Debug.toString (round model.savedTime) ++ " Seconds") ]
                    ]
                ]
            ]
        , container []
            [ level
                []
                [ levelItem [] [ stateButton model Working ]
                , levelItem [] [ stateButton model Breaking ]
                , levelItem [] [ stateButton model Paused ]
                ]
            ]
        , section NotSpaced [] (hr [] [] :: List.map showHistoryItem model.history)
        ]


paceOption : Model -> Pace -> Html Msg
paceOption model p =
    option
        [ onClick (Select p)
        , selected (model.pace == p)
        ]
        [ text (description p) ]


stateButton : Model -> Working -> Html Msg
stateButton model w =
    button
        { buttonModifiers
            | color = workingColor w
        }
        [ onClick (ChangeState w)
        , disabled
            (if model.state == w then
                True

             else
                False
            )
        ]
        [ text (workingLabel w) ]


showHistoryItem : HistoryItem -> Html Msg
showHistoryItem item =
    -- TODO: We need better formatting all throughout this function.
    container []
        [ level
            [ displayByDevice
                { mobile = Hidden
                , tablet = Flex
                , desktop = Flex
                , widescreen = Flex
                , fullHD = Flex
                }
            ]
            [ levelItem
                []
                [ historyLevelItem item Working ]
            , levelItem
                []
                [ historyLevelItem item Breaking ]
            , levelItem
                []
                [ historyLevelItem item Paused ]
            ]
        , level
            [ displayByDevice
                { mobile = Flex
                , tablet = Hidden
                , desktop = Hidden
                , widescreen = Hidden
                , fullHD = Hidden
                }
            ]
            [ levelItem
                []
                [ text (Debug.toString (Maybe.map2 timeDiff item.endTime item.startTime)) ]
            ]
        ]


historyLevelItem : HistoryItem -> Working -> Html msg
historyLevelItem item state =
    if item.state == state then
        text (Debug.toString (Maybe.map2 timeDiff item.endTime item.startTime))

    else
        text ""



-- HELPERS


{-| disabled is a workaround of the fact that the normal disabled function only
affects <button> tags, but the bulma library uses <a> tags for them. This is a
workaround that hopefully isn't broken in some subtle way.
-}
disabled : Bool -> Html.Attribute msg
disabled t =
    if t then
        attribute "disabled" ""

    else
        -- A dummy do-nothing attribute to have a value to return here
        attribute "style" ""


timeDiff : Time.Posix -> Time.Posix -> Float
timeDiff a b =
    toFloat (Time.posixToMillis a - Time.posixToMillis b) / 1000
