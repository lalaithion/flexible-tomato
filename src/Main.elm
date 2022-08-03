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


main : Program () Model Msg
main =
    Browser.element
        { init = \_ -> ( { savedTime = 0, lastTime = Nothing, startTime = Nothing, state = Paused, ratio = 0.33, history = [] }, Cmd.none )
        , update = update
        , subscriptions = subscriptions
        , view = view
        }


type Msg
    = Tick Time.Posix
    | Select Pace
    | StartWorking
    | StartBreaking
    | Pause


type Pace
    = Langorious
    | Relaxed
    | Standard
    | Industrious
    | Grinding


type Working
    = Working
    | Breaking
    | Paused


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
    , ratio {- Between 0 and 1, indicates the percent of time worked that is saved for breaks -} : Float
    , history : List HistoryItem
    }


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
        Select Langorious ->
            { model | ratio = 1 }

        Select Relaxed ->
            { model | ratio = 1 / 2 }

        Select Standard ->
            { model | ratio = 1 / 3 }

        Select Industrious ->
            { model | ratio = 1 / 5 }

        Select Grinding ->
            { model | ratio = 1 / 11 }

        Tick currentTime ->
            case model.state of
                Working ->
                    updateTimes (\oldSaved oldTime -> oldSaved + timeDiff currentTime oldTime * model.ratio) currentTime model

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

        StartBreaking ->
            if model.state /= Breaking then
                { model | state = Breaking, startTime = model.lastTime, history = { state = model.state, endTime = model.lastTime, startTime = model.startTime } :: model.history }

            else
                model

        StartWorking ->
            if model.state /= Working then
                { model | state = Working, startTime = model.lastTime, history = { state = model.state, endTime = model.lastTime, startTime = model.startTime } :: model.history }

            else
                model

        Pause ->
            if model.state /= Paused then
                { model | state = Paused, startTime = model.lastTime, history = { state = model.state, endTime = model.lastTime, startTime = model.startTime } :: model.history }

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


subscriptions : Model -> Sub Msg
subscriptions model =
    Time.every 30 Tick


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


view : Model -> Html Msg
view model =
    -- TODO: Can this be broken up at all?
    main_ []
        [ stylesheet
        , level []
            [ levelItem [ textCentered ]
                [ select [ value "Standard (spend 1/4 of your time on break)" ]
                    [ option [ onClick (Select Langorious), selected (model.ratio == 1) ] [ text "Langorious (break for as long as you work)" ]
                    , option [ onClick (Select Relaxed), selected (model.ratio == 1 / 2) ] [ text "Relaxed (spend 1/3 of your time on break)" ]
                    , option [ onClick (Select Standard), selected (model.ratio == 1 / 3) ] [ text "Standard (spend 1/4 of your time on break)" ]
                    , option [ onClick (Select Industrious), selected (model.ratio == 1 / 5) ] [ text "Industrious (spend 1/6 of your time on break)" ]
                    , option [ onClick (Select Grinding), selected (model.ratio == 1 / 11) ] [ text "Grinding (spend 1/12 of your time on break)" ]
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
                [ levelItem
                    []
                    [ button
                        { buttonModifiers
                            | color = Danger
                        }
                        [ onClick StartWorking
                        , disabled
                            (if model.state == Working then
                                True

                             else
                                False
                            )
                        ]
                        [ text "🛠️ Start working" ]
                    ]
                , levelItem
                    []
                    [ button
                        { buttonModifiers
                            | color = Success
                        }
                        [ onClick StartBreaking
                        , disabled
                            (if model.state == Breaking then
                                True

                             else
                                False
                            )
                        ]
                        [ text "🏖️ Take a break" ]
                    ]
                , levelItem
                    []
                    [ button
                        { buttonModifiers
                            | color = Info
                        }
                        [ onClick Pause
                        , disabled
                            (if model.state == Paused then
                                True

                             else
                                False
                            )
                        ]
                        [ text "🔒 (Pause the timer)" ]
                    ]
                ]
            ]
        , section NotSpaced [] (hr [] [] :: List.map showHistoryItem model.history)
        ]


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
                [ if item.state == Working then
                    text (Debug.toString (Maybe.map2 timeDiff item.endTime item.startTime))

                  else
                    text ""
                ]
            , levelItem
                []
                [ if item.state == Breaking then
                    text (Debug.toString (Maybe.map2 timeDiff item.endTime item.startTime))

                  else
                    text ""
                ]
            , levelItem
                []
                [ if item.state == Paused then
                    text (Debug.toString (Maybe.map2 timeDiff item.endTime item.startTime))

                  else
                    text ""
                ]
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


timeDiff : Time.Posix -> Time.Posix -> Float
timeDiff a b =
    toFloat (Time.posixToMillis a - Time.posixToMillis b) / 1000
