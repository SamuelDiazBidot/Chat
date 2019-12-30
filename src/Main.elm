module Main exposing (..)

import Browser
import Html exposing (Html, text, div, h1, img, form, input, button)
import Html.Attributes exposing (src, type_, placeholder, disabled, class, value)
import Html.Events exposing (onInput, onSubmit, onClick)
import Json.Decode exposing (Decoder, succeed, int, string, bool, decodeString) 
import Json.Decode.Pipeline exposing (required)
import Json.Encode as Encode
import WebSockets

---- MODEL ----
type alias Message =
    { userName : String
    , message : String 
    , uid : Int
    , deleted : Bool
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

type alias Model =
    { messages : List Message 
    , currentMessage : String 
    , userName : String
    , currentUserName : String
    }

initialModel : Model 
initialModel = 
    { messages = []
    , currentMessage = ""
    , userName = "" 
    , currentUserName = ""
    }

init : () -> ( Model, Cmd Msg )
init () =
    ( initialModel, Cmd.none )

---- UPDATE ----
type Msg
    = UpdateCurrentMessage String
    | AddMessage (Result Json.Decode.Error Message)
    | SendMessage Message
    | Delete Int
    | SendDeleteRequest Int
    | SubmitUserName String
    | UpdateCurrentUserName String

update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of 
        UpdateCurrentMessage message ->
            ( { model | currentMessage = message }, Cmd.none )
        AddMessage (Ok message) ->
            ( { model | messages = message :: model.messages }
            , Cmd.none 
            )
        AddMessage (Err error) ->
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
    }

---- SUBSCRIPTIONS ---- 
subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch 
        [ WebSockets.addMessageIn (AddMessage << decodeString messageDecoder) 
        , WebSockets.deleteMessageIn (Delete << (\t -> Maybe.withDefault 0 <| String.toInt t))
        ]

---- VIEW ----
viewUsernameSelection : Model -> Html Msg 
viewUsernameSelection model =
    div []
        [ form [onSubmit ( SubmitUserName model.currentUserName )] 
            [ input [ type_ "text" 
                    , placeholder "Enter your user name"
                    , onInput UpdateCurrentUserName
                    ] 
                    []
            , button [ disabled (String.isEmpty model.currentUserName)] [ text "Continue"]]]

viewInputArea : Model -> Html Msg
viewInputArea model = 
    div [] 
        [ form [ onSubmit (SendMessage <| newMessage model.userName model.currentMessage)] 
            [ input 
                [ type_ "text"
                , onInput UpdateCurrentMessage
                , value model.currentMessage
                , class "message-input" 
                ] 
                [] 
            , button 
                [ disabled (String.isEmpty model.currentMessage)
                , class "send-button" 
                ] 
                [ text "Send" ] 
            ]
        ]

viewReceivedMessage : Message -> Html Msg
viewReceivedMessage message =
    div [ class "received-message"]
        [ div [ class "received-message-username" ] [ text (message.userName ++ ":") ]
        , div [ class "received-message-message" ] [ text message.message ]
        ]

viewSentMessage : Message -> Html Msg
viewSentMessage message =
    div [ class "sent-message"]
        [ div [] [ text message.message ] 
        , button
            [ onClick (SendDeleteRequest message.uid)
            , class "delete-button" 
            ] 
            [ text "delete" ]
        ]

viewMessage : String -> Message -> Html Msg
viewMessage username message =
    if message.deleted then 
        div []
            [ text (message.userName ++ " deleted this message") ]
    else
        if message.userName == username then 
            viewSentMessage message
        else 
            viewReceivedMessage message

---- VIEW ----
view : Model -> Html Msg
view model =
    case model.userName of 
        "" -> 
            viewUsernameSelection model
        _ ->
            div []
                [ h1 [] [ text "Chat App" ]
                , viewInputArea model
                , div [] (List.map (viewMessage model.userName) model.messages) 
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
