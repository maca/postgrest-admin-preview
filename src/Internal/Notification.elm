module Internal.Notification exposing
    ( Msg
    , Notification
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
import Task exposing (Task)
import Time


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
    task (Alert message)


confirm : String -> Task Never Msg
confirm message =
    task (Confirm message)


dismiss : Task Never Msg
dismiss =
    task Dismiss


task : Msg -> Task Never Msg
task msg =
    Time.now |> Task.andThen (always <| Task.succeed msg)


update : Msg -> Notification
update msg =
    case msg of
        Confirm message ->
            Confirmation message

        Alert message ->
            Error message

        Dismiss ->
            None


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
    div [ class "notification", class notificationType ]
        [ p [] [ text message ]
        , div [ class "close" ]
            [ i [ class "icono-cross", onClick Dismiss ] [] ]
        ]
