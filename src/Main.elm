module Main exposing (..)

import Browser
import Html exposing (Html, text, div, h1, form, input, button)
import Html.Attributes exposing ( type_, placeholder, disabled, class, value)
import Html.Events exposing (onInput, onSubmit, onClick)
import Json.Decode exposing (Decoder, succeed, int, string, bool, decodeString)
import Json.Decode.Pipeline exposing (required)
import Json.Encode as Encode
import WebSockets
import Task
import Time

---- MODEL ----
type alias Message =
    { userName : String
    , message : String 
    , uid : Int
    , deleted : Bool
    , edited : Bool
    }

messageEncoder : Message -> Encode.Value
messageEncoder message =
    Encode.object
        [ ("userName", Encode.string message.userName)
        , ("message", Encode.string message.message)
        , ("uid", Encode.int message.uid)
        , ("deleted", Encode.bool message.deleted)
        ]

messageDecoder : Decoder Message
messageDecoder = 
    succeed Message
        |> required "userName" string
        |> required "message" string 
        |> required "uid" int 
        |> required "deleted" bool
        |> required "edited" bool

type alias Model =
    { messages : List Message 
    , currentMessage : String 
    , userName : String
    , currentUserName : String
    , time : Time.Posix
    , zone : Time.Zone
    }

initialModel : Model 
initialModel = 
    { messages = []
    , currentMessage = ""
    , userName = "" 
    , currentUserName = ""
    , time = Time.millisToPosix 0
    , zone = Time.utc
    }

init : () -> ( Model, Cmd Msg )
init () =
    ( initialModel 
    , Task.perform AdjustTimeZone Time.here 
    )

---- UPDATE ----
type Msg
    = UpdateCurrentMessage String
    | AddMessage (Result Json.Decode.Error Message)
    | SendMessage Message
    | Delete Int
    | SendDeleteRequest Int
    | SubmitUserName String
    | UpdateCurrentUserName String
    | NewTime Time.Posix
    | AdjustTimeZone Time.Zone

update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of 
        UpdateCurrentMessage message ->
            ( { model | currentMessage = message }, Cmd.none )
        AddMessage (Ok message) ->
            ( { model | messages = message :: model.messages }
            , getNewTime 
            )
        AddMessage (Err _) ->
            (model, Cmd.none)
        SendMessage message ->
            ( { model | currentMessage = "" }
            , WebSockets.addMessageOut (Encode.encode 0 (messageEncoder message)) )
        Delete id ->
            ( { model | messages = List.map (\message -> deleteById id message) model.messages }
            , Cmd.none
            )
        SendDeleteRequest id ->
            ( model
            , WebSockets.deleteMessageOut (String.fromInt id) 
            )
        SubmitUserName username ->
            ( { model | userName = username }, Cmd.none )
        UpdateCurrentUserName username ->
            ( { model | currentUserName = username }, Cmd.none )
        NewTime newTime ->
            ( { model | time = newTime } 
            , Cmd.none 
            )
        AdjustTimeZone newZone ->
            ( { model | zone = newZone }
            , Cmd.none 
            )

deleteById : Int -> Message -> Message
deleteById id message = 
    if message.uid == id then 
        { message | deleted = True }
    else 
        message

newMessage : String -> String -> Message
newMessage userName message = 
    { userName = userName 
    , message = message
    , uid = 0 
    , deleted = False
    , edited = False
    }

getNewTime : Cmd Msg 
getNewTime =
    Task.perform NewTime Time.now

---- SUBSCRIPTIONS ---- 
subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.batch 
        [ WebSockets.addMessageIn (AddMessage << decodeString messageDecoder) 
        , WebSockets.deleteMessageIn (Delete << (\t -> Maybe.withDefault 0 <| String.toInt t))
        ]

---- VIEW ----
viewUsernameSelection : Model -> Html Msg 
viewUsernameSelection model =
    div [ class "username-div"]
        [ form [onSubmit ( SubmitUserName model.currentUserName )] 
            [ input [ type_ "text" 
                    , placeholder "Enter your user name"
                    , onInput UpdateCurrentUserName
                    , class "username-input"
                    ] 
                    []
            , button 
                    [ disabled (String.isEmpty <| String.trim model.currentUserName)
                    , class "username-continue-button"
                    ] 
                    [ text "Continue"]
            ]
        ]

viewInputArea : Model -> Html Msg
viewInputArea model = 
    div [ class "input-area" ] 
        [ form [ onSubmit (SendMessage <| newMessage model.userName model.currentMessage)] 
            [ input 
                [ type_ "text"
                , onInput UpdateCurrentMessage
                , value model.currentMessage
                , class "message-input" 
                ] 
                [] 
            , button 
                [ disabled (String.isEmpty <| String.trim model.currentMessage)
                , class "send-button" 
                ] 
                [ text "Send" ] 
            ]
        ]

viewTime : Time.Zone -> Time.Posix -> Html Msg
viewTime zone time =
    let
        hourInt = Time.toHour zone time 
        hour = 
            if hourInt >= 12 then 
                String.fromInt (hourInt - 12)
            else 
                String.fromInt hourInt
        minute = String.fromInt (Time.toMinute zone time)
        meridiem =
            if (hourInt >= 12) && (hourInt < 24) then 
                "pm"
            else 
                "am"
    in
        text (hour ++ ":" ++ minute ++ " " ++ meridiem)

viewReceivedMessage : Message -> Time.Zone -> Time.Posix -> Html Msg
viewReceivedMessage message zone time =
    div [ class "received-message"]
        [ div [ class "received-message-username" ] [ text (message.userName ++ ":") ]
        , div [ class "received-message-message" ] [ text message.message ]
        , div [ class "received-message-time" ] [ viewTime zone time ]
        ]

viewSentMessage : Message -> Time.Zone -> Time.Posix -> Html Msg
viewSentMessage message zone time =
    div [ class "sent-message"]
        [ div [ class "sent-message-message" ] [ text message.message ] 
        , button
            [ onClick (SendDeleteRequest message.uid)
            , class "delete-button" 
            ] 
            [ text "delete" ]
        , div [ class "sent-message-time" ] [ viewTime zone time ]
        ]

viewMessage : String -> Time.Zone -> Time.Posix -> Message -> Html Msg
viewMessage username zone time message =
    if message.deleted then 
        div []
            [ text (message.userName ++ " deleted this message") ]
    else
        if message.userName == username then 
            viewSentMessage message zone time
        else 
            viewReceivedMessage message zone time

---- VIEW ----
viewBody : Model -> Html Msg
viewBody model =
    case model.userName of 
        "" ->
            viewUsernameSelection model
        _ ->
            div []
                [ div [ class "messages-div" ] (List.map (viewMessage model.userName model.zone model.time) model.messages)
                , viewInputArea model
                ]

view : Model -> Html Msg
view model =
            div []
                [ h1 [] [ text "Chat App" ]
                , viewBody model
                ]

---- PROGRAM ----
main : Program () Model Msg
main =
    Browser.element
        { view = view
        , init = init
        , update = update
        , subscriptions = subscriptions
        }
