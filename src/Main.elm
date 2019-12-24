module Main exposing (..)

import Browser
import Html exposing (Html, text, div, h1, img, form, input, button)
import Html.Attributes exposing (src, type_, placeholder, disabled)
import Html.Events exposing (onInput, onSubmit, onClick)
import Json.Decode exposing (Decoder, succeed, int, string, bool) 
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
    , id : Int
    }

initialModel : Model 
initialModel = 
    { messages = [{userName = "Monica", message = "Whats up", uid = 1, deleted = False}]
    , currentMessage = ""
    , userName = "" 
    , currentUserName = ""
    , id = 0
    }

init : ( Model, Cmd Msg )
init =
    ( initialModel, Cmd.none )

---- UPDATE ----
type Msg
    = UpdateCurrentMessage String
    | AddMessage Message
    | Delete Int
    | SubmitUserName String
    | UpdateCurrentUserName String

update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of 
        UpdateCurrentMessage message ->
            ( { model | currentMessage = message }, Cmd.none )
        AddMessage message ->
            ( { model | id = model.id + 1
                      , messages = message :: model.messages 
              }
            , Cmd.none 
            )
        Delete id ->
            ( { model | messages = List.map (\message -> deleteById id message) model.messages }
            , Cmd.none
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

newMessage : String -> String -> Int -> Message
newMessage userName message id = 
    { userName = userName 
    , message = message
    , uid = id
    , deleted = False
    }

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
        [ form [ onSubmit (AddMessage <| newMessage model.userName model.currentMessage model.id)] 
            [ input [ type_ "text", onInput UpdateCurrentMessage ] [ ] 
            , button [ disabled (String.isEmpty model.currentMessage) ] [ text "Send" ] 
            ]
        ]

viewReceivedMessage : Message -> Html Msg
viewReceivedMessage message =
    div []
        [ text (message.userName ++ ": " ++ message.message)]

viewSentMessage : Message -> Html Msg
viewSentMessage message =
    div []
        [ text message.message 
        , button [ onClick (Delete message.uid) ] [ text "delete" ]
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
        , init = \_ -> init
        , update = update
        , subscriptions = always Sub.none
        }
