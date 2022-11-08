module Internal.Notification exposing
    ( Msg
    , Notification(..)
    , confirm
    , dismiss
    , error
    , none
    , update
    , view
    )

import Html exposing (Html, div, i, pre, text)
import Html.Attributes exposing (class)
import Html.Events exposing (onClick)
import Process
import Task exposing (Task)


type Msg
    = Dismiss
    | Confirm String
    | Alert String


type Notification
    = Confirmation String
    | Error String
    | None


none : Notification
none =
    None


error : String -> Task Never Msg
error message =
    Task.succeed (Alert message)


confirm : String -> Task Never Msg
confirm message =
    Task.succeed (Confirm message)


dismiss : Task Never Msg
dismiss =
    Task.succeed Dismiss


update : Msg -> ( Notification, Cmd Msg )
update msg =
    case msg of
        Confirm message ->
            ( Confirmation message
            , Process.sleep 5000
                |> Task.perform (always Dismiss)
            )

        Alert message ->
            ( Error message, Cmd.none )

        Dismiss ->
            ( None, Cmd.none )


view : Notification -> Html Msg
view notification =
    case notification of
        Confirmation text ->
            viewHelp "confirmation" text

        Error text ->
            viewHelp "error" text

        None ->
            text ""


viewHelp : String -> String -> Html Msg
viewHelp notificationType message =
    div
        [ class "notification"
        , class notificationType
        ]
        [ pre [] [ text message ]
        , div [ class "close" ]
            [ i [ class "icono-cross", onClick Dismiss ] [] ]
        ]
