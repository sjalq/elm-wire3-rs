port module Main exposing (main)

import Browser
import Bytes exposing (Bytes)
import Bytes.Decode as BD
import Bytes.Encode as BE
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick, onInput, onSubmit)
import Lamdera.Wire3 as Wire3
import Messages exposing (..)


-- PORTS

port wire3WsConnect : String -> Cmd msg

port wire3WsDisconnect : () -> Cmd msg

port wire3WsSend : List Int -> Cmd msg

port wire3WsReceived : (List Int -> msg) -> Sub msg

port wire3WsStatus : (String -> msg) -> Sub msg



-- MODEL


type alias Flags =
    { wsUrl : String }


type alias LogEntry =
    { direction : String -- "<-" or "->"
    , message : String
    , byteCount : Int
    }


type alias Model =
    { tasks : List Task
    , input : String
    , status : String
    , log : List LogEntry
    }


init : Flags -> ( Model, Cmd Msg )
init flags =
    ( { tasks = []
      , input = ""
      , status = "connecting"
      , log = []
      }
    , wire3WsConnect flags.wsUrl
    )



-- UPDATE


type Msg
    = SendToBackend ToBackend
    | InputChanged String
    | SubmitTask
    | GotWsMessage (List Int)
    | GotWsStatus String


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        SendToBackend toBackend ->
            let
                bytes =
                    BE.encode (w3_encode_ToBackend toBackend)

                byteList =
                    bytesToList bytes

                entry =
                    { direction = "<-"
                    , message = describeToBackend toBackend
                    , byteCount = List.length byteList
                    }
            in
            ( { model | log = entry :: model.log }
            , wire3WsSend byteList
            )

        InputChanged val ->
            ( { model | input = val }, Cmd.none )

        SubmitTask ->
            if String.trim model.input == "" then
                ( model, Cmd.none )

            else
                let
                    trimmed =
                        String.trim model.input
                in
                update (SendToBackend (AddTask trimmed)) { model | input = "" }

        GotWsMessage byteList ->
            let
                bytes =
                    listToBytes byteList
            in
            case BD.decode w3_decode_ToFrontend bytes of
                Just toFrontend ->
                    let
                        entry =
                            { direction = "->"
                            , message = describeToFrontend toFrontend
                            , byteCount = List.length byteList
                            }

                        newModel =
                            { model | log = entry :: model.log }
                    in
                    applyToFrontend toFrontend newModel

                Nothing ->
                    ( model, Cmd.none )

        GotWsStatus status ->
            let
                newModel =
                    { model | status = status }
            in
            if status == "connected" then
                update (SendToBackend RequestTasks) newModel

            else
                ( newModel, Cmd.none )


applyToFrontend : ToFrontend -> Model -> ( Model, Cmd Msg )
applyToFrontend msg model =
    case msg of
        TaskList tasks ->
            ( { model | tasks = tasks }, Cmd.none )

        TaskAdded task ->
            ( { model | tasks = model.tasks ++ [ task ] }, Cmd.none )

        TaskToggled id completed ->
            let
                toggle t =
                    if t.id == id then
                        { t | completed = completed }

                    else
                        t
            in
            ( { model | tasks = List.map toggle model.tasks }, Cmd.none )

        TaskDeleted id ->
            ( { model | tasks = List.filter (\t -> t.id /= id) model.tasks }, Cmd.none )



-- DESCRIBE MESSAGES (for log panel)


describeToBackend : ToBackend -> String
describeToBackend msg =
    case msg of
        AddTask title ->
            "AddTask \"" ++ title ++ "\""

        ToggleTask id ->
            "ToggleTask " ++ String.fromInt id

        DeleteTask id ->
            "DeleteTask " ++ String.fromInt id

        RequestTasks ->
            "RequestTasks"


describeToFrontend : ToFrontend -> String
describeToFrontend msg =
    case msg of
        TaskList tasks ->
            "TaskList [" ++ String.fromInt (List.length tasks) ++ " tasks]"

        TaskAdded task ->
            "TaskAdded { id=" ++ String.fromInt task.id ++ ", \"" ++ task.title ++ "\" }"

        TaskToggled id completed ->
            "TaskToggled " ++ String.fromInt id ++ " " ++ boolStr completed

        TaskDeleted id ->
            "TaskDeleted " ++ String.fromInt id


boolStr : Bool -> String
boolStr b =
    if b then
        "True"

    else
        "False"



-- BYTES <-> LIST INT CONVERSION


bytesToList : Bytes -> List Int
bytesToList bytes =
    BD.decode (bytesListDecoder (Bytes.width bytes)) bytes
        |> Maybe.withDefault []


bytesListDecoder : Int -> BD.Decoder (List Int)
bytesListDecoder width =
    BD.loop ( width, [] ) bytesListStep


bytesListStep : ( Int, List Int ) -> BD.Decoder (BD.Step ( Int, List Int ) (List Int))
bytesListStep ( remaining, acc ) =
    if remaining <= 0 then
        BD.succeed (BD.Done (List.reverse acc))

    else
        BD.unsignedInt8
            |> BD.map (\byte -> BD.Loop ( remaining - 1, byte :: acc ))


listToBytes : List Int -> Bytes
listToBytes ints =
    BE.encode (BE.sequence (List.map BE.unsignedInt8 ints))



-- VIEW


view : Model -> Html Msg
view model =
    div [ style "font-family" "'SF Mono', 'Fira Code', monospace"
        , style "background" "#0a0e14"
        , style "color" "#c5c8c6"
        , style "min-height" "100vh"
        , style "display" "flex"
        , style "justify-content" "center"
        , style "padding" "2em"
        ]
        [ div [ style "max-width" "700px", style "width" "100%" ]
            [ viewHeader model.status
            , viewInput model.input
            , viewTasks model.tasks
            , viewLog model.log
            ]
        ]


viewHeader : String -> Html msg
viewHeader status =
    div [ style "margin-bottom" "1.5em" ]
        [ h1 [ style "color" "#00d4aa", style "font-size" "1.4em", style "margin" "0 0 0.3em 0" ]
            [ text "Wire3 Task Manager" ]
        , span
            [ style "font-size" "0.75em"
            , style "color"
                (case status of
                    "connected" ->
                        "#00d4aa"

                    "reconnecting" ->
                        "#f0c674"

                    _ ->
                        "#cc6666"
                )
            ]
            [ text status ]
        , span [ style "font-size" "0.75em", style "color" "#5c6370", style "margin-left" "1em" ]
            [ text "binary Wire3 over WebSocket" ]
        ]


viewInput : String -> Html Msg
viewInput val =
    Html.form [ onSubmit SubmitTask, style "display" "flex", style "gap" "0.5em", style "margin-bottom" "1.5em" ]
        [ input
            [ placeholder "Add a task..."
            , value val
            , onInput InputChanged
            , style "flex" "1"
            , style "padding" "0.6em 0.8em"
            , style "background" "#1a1f29"
            , style "border" "1px solid #2a2f39"
            , style "color" "#c5c8c6"
            , style "border-radius" "4px"
            , style "font-family" "inherit"
            , style "font-size" "0.9em"
            , style "outline" "none"
            ]
            []
        , button
            [ type_ "submit"
            , style "padding" "0.6em 1.2em"
            , style "background" "#00d4aa"
            , style "color" "#0a0e14"
            , style "border" "none"
            , style "border-radius" "4px"
            , style "cursor" "pointer"
            , style "font-family" "inherit"
            , style "font-weight" "bold"
            , style "font-size" "0.9em"
            ]
            [ text "Add" ]
        ]


viewTasks : List Task -> Html Msg
viewTasks tasks =
    if List.isEmpty tasks then
        p [ style "color" "#5c6370", style "font-size" "0.85em", style "padding" "1em 0" ]
            [ text "No tasks yet. Add one above." ]

    else
        div [ style "margin-bottom" "1.5em" ] (List.map viewTask tasks)


viewTask : Task -> Html Msg
viewTask task =
    div
        [ style "display" "flex"
        , style "align-items" "center"
        , style "padding" "0.5em 0.8em"
        , style "margin-bottom" "0.3em"
        , style "background" "#1a1f29"
        , style "border-radius" "4px"
        , style "border" "1px solid #2a2f39"
        ]
        [ span
            [ onClick (SendToBackend (ToggleTask task.id))
            , style "cursor" "pointer"
            , style "margin-right" "0.8em"
            , style "font-size" "1.1em"
            , style "user-select" "none"
            ]
            [ text
                (if task.completed then
                    "[x]"

                 else
                    "[ ]"
                )
            ]
        , span
            [ style "flex" "1"
            , style "color"
                (if task.completed then
                    "#5c6370"

                 else
                    "#c5c8c6"
                )
            , style "text-decoration"
                (if task.completed then
                    "line-through"

                 else
                    "none"
                )
            ]
            [ text task.title ]
        , span
            [ style "font-size" "0.7em", style "color" "#5c6370", style "margin-right" "0.8em" ]
            [ text ("#" ++ String.fromInt task.id) ]
        , span
            [ onClick (SendToBackend (DeleteTask task.id))
            , style "cursor" "pointer"
            , style "color" "#cc6666"
            , style "font-size" "0.9em"
            , style "user-select" "none"
            ]
            [ text "x" ]
        ]


viewLog : List LogEntry -> Html msg
viewLog entries =
    div []
        [ div
            [ style "color" "#5c6370"
            , style "font-size" "0.7em"
            , style "text-transform" "uppercase"
            , style "letter-spacing" "0.1em"
            , style "margin-bottom" "0.5em"
            ]
            [ text "Wire3 message log" ]
        , div
            [ style "background" "#0d1117"
            , style "border" "1px solid #1a1f29"
            , style "border-radius" "4px"
            , style "padding" "0.5em"
            , style "max-height" "200px"
            , style "overflow-y" "auto"
            , style "font-size" "0.75em"
            ]
            (if List.isEmpty entries then
                [ span [ style "color" "#5c6370" ] [ text "Waiting for messages..." ] ]

             else
                List.map viewLogEntry entries
            )
        ]


viewLogEntry : LogEntry -> Html msg
viewLogEntry entry =
    div [ style "padding" "0.15em 0.3em", style "white-space" "nowrap" ]
        [ span
            [ style "color"
                (if entry.direction == "<-" then
                    "#61afef"

                 else
                    "#98c379"
                )
            ]
            [ text entry.direction ]
        , span [ style "margin-left" "0.5em", style "color" "#abb2bf" ]
            [ text entry.message ]
        , span [ style "margin-left" "0.5em", style "color" "#5c6370" ]
            [ text ("[" ++ String.fromInt entry.byteCount ++ "B]") ]
        ]



-- MAIN


main : Program Flags Model Msg
main =
    Browser.element
        { init = init
        , update = update
        , subscriptions =
            \_ ->
                Sub.batch
                    [ wire3WsReceived GotWsMessage
                    , wire3WsStatus GotWsStatus
                    ]
        , view = view
        }
