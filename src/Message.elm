module Message exposing
    ( Message
    , Msg
    , confirm
    , dismiss
    , error
    , none
    , update
    , view
    )

import Html exposing (Html, div, i, p, text)
import Html.Attributes exposing (class)
import Html.Events exposing (onClick)


type Msg
    = Dismiss


type Message
    = Confirmation String
    | Error String
    | None


error : String -> Message
error message =
    Error message


confirm : String -> Message
confirm message =
    Confirmation message


none : Message
none =
    None


dismiss : Msg
dismiss =
    Dismiss


update : Msg -> Message -> ( Message, Cmd Msg )
update Dismiss _ =
    ( None, Cmd.none )


view : Message -> Html Msg
view message =
    case message of
        Confirmation text ->
            viewHelp "confirmation" text

        Error text ->
            viewHelp "error" text

        None ->
            text ""


viewHelp : String -> String -> Html Msg
viewHelp messageType message =
    div [ class "message", class messageType ]
        [ p [] [ text message ]
        , div [ class "close" ]
            [ i [ class "icono-cross", onClick Dismiss ] [] ]
        ]
