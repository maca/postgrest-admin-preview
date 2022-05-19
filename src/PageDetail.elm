module PageDetail exposing (Msg, PageDetail, init, mapMsg, update, view)

import Browser.Navigation as Nav
import Dict
import Html
    exposing
        ( Html
        , a
        , article
        , aside
        , button
        , div
        , h1
        , h2
        , p
        , section
        , table
        , td
        , text
        , th
        , tr
        )
import Html.Attributes exposing (class, href)
import Html.Events exposing (onClick)
import Notification
import Postgrest.Field as Field exposing (Field)
import Postgrest.Record as Record exposing (Record)
import Postgrest.Record.Client as Client exposing (Client)
import Postgrest.Schema exposing (Constraint(..), Reference, Schema)
import Postgrest.Value exposing (Value(..))
import PostgrestAdmin.OuterMsg as OuterMsg exposing (OuterMsg)
import String.Extra as String
import Task exposing (Task)
import Url.Builder as Url
import Utils.Task exposing (Error(..), attemptWithError)


type Msg
    = DeleteModalOpened
    | DeleteModalClosed
    | DeleteConfirmed
    | Deleted
    | NotificationChanged Notification.Msg
    | Failed Error


type PageDetail
    = PageDetail { record : Record, confirmDelete : Bool }


init : Record -> PageDetail
init record =
    PageDetail { record = record, confirmDelete = False }


update :
    Client { a | key : Nav.Key }
    -> Msg
    -> PageDetail
    -> ( PageDetail, Cmd Msg )
update client msg (PageDetail params) =
    case msg of
        DeleteModalOpened ->
            ( PageDetail { params | confirmDelete = True }, Cmd.none )

        DeleteModalClosed ->
            ( PageDetail { params | confirmDelete = False }, Cmd.none )

        DeleteConfirmed ->
            ( PageDetail params
            , Client.delete client (Record.toTable params.record) params.record
                |> attemptWithError Failed (always Deleted)
            )

        Deleted ->
            ( PageDetail params
            , Notification.confirm "The record was deleted"
                |> navigate client.key (Record.tableName params.record)
            )

        NotificationChanged _ ->
            ( PageDetail params, Cmd.none )

        Failed _ ->
            ( PageDetail params, Cmd.none )


navigate : Nav.Key -> String -> Task Never Notification.Msg -> Cmd Msg
navigate key resourcesName notificationTask =
    Cmd.batch
        [ Url.absolute [ resourcesName ] [] |> Nav.pushUrl key
        , Task.perform NotificationChanged notificationTask
        ]


mapMsg : Msg -> OuterMsg
mapMsg msg =
    case msg of
        Failed err ->
            OuterMsg.RequestFailed err

        NotificationChanged innerMsg ->
            OuterMsg.NotificationChanged innerMsg

        _ ->
            OuterMsg.Pass



-- View


view : Schema -> PageDetail -> Html Msg
view schema (PageDetail { record, confirmDelete }) =
    section
        [ class "record-detail" ]
        [ h1
            []
            [ Record.label record
                |> Maybe.withDefault ""
                |> (++) (String.humanize (Record.tableName record) ++ " - ")
                |> text
            ]
        , article
            [ class "card" ]
            [ table
                []
                (sortedFields record |> List.map (tableRow (Record.tableName record)))
            , actions record
            ]
        , if confirmDelete then
            div
                [ class "modal-background" ]
                [ div
                    [ class "modal-dialog" ]
                    [ h2 []
                        [ text """Are you sure you want to delete the
                                          record?"""
                        ]
                    , p [] [ text "This action cannot be undone." ]
                    , div
                        [ class "actions" ]
                        [ button
                            [ class "button button-danger"
                            , onClick DeleteConfirmed
                            ]
                            [ text "Delete" ]
                        , button
                            [ class "button"
                            , onClick DeleteModalClosed
                            ]
                            [ text "Cancel" ]
                        ]
                    ]
                ]

          else
            text ""
        , aside
            [ class "associations" ]
            (Record.referencedBy schema record
                |> List.map referenceToHtml
            )
        ]


referenceToHtml : Reference -> Html Msg
referenceToHtml { foreignKeyName, foreignKeyValue, table } =
    a
        [ class "card association"
        , href
            ("/"
                ++ table.name
                ++ "?"
                ++ foreignKeyName
                ++ "=eq."
                ++ foreignKeyValue
            )
        ]
        [ text (String.humanize table.name) ]


actions : Record -> Html Msg
actions record =
    case Record.id record of
        Just id ->
            div
                [ class "actions" ]
                [ a
                    [ href
                        (Url.absolute [ Record.tableName record, id, "edit" ] [])
                    , class "button"
                    ]
                    [ text "Edit" ]
                , button
                    [ onClick DeleteModalOpened
                    , class "button button-danger"
                    ]
                    [ text "Delete" ]
                ]

        Nothing ->
            text ""


tableRow : String -> ( String, Field ) -> Html Msg
tableRow resourcesName ( name, field ) =
    tr
        []
        [ th [] [ text (String.humanize name) ]
        , td [] [ Field.toHtml (\_ _ -> class "") resourcesName field ]
        ]



-- Utils


sortedFields : Record -> List ( String, Field )
sortedFields record =
    Dict.toList record.fields
        |> List.sortWith Field.compareTuple
