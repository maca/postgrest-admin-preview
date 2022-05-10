module Detail exposing (Detail, Msg, fetch, init, mapMsg, update, view)

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
import Postgrest.Schema exposing (Constraint(..), Reference, Schema, Table)
import Postgrest.Value exposing (Value(..))
import PostgrestAdmin.OuterMsg as OuterMsg exposing (OuterMsg)
import String.Extra as String
import Task exposing (Task)
import Url.Builder as Url
import Utils.Task exposing (Error(..), attemptWithError)


type Msg
    = Fetched Record
    | DeleteModalOpened
    | DeleteModalClosed
    | DeleteConfirmed
    | Deleted
    | NotificationChanged Notification.Msg
    | Failed Error


type Detail
    = Detail
        { table : Table
        , id : String
        , record : Maybe Record
        , confirmDelete : Bool
        }


init : Table -> String -> Detail
init table id =
    Detail
        { table = table
        , id = id
        , confirmDelete = False
        , record = Nothing
        }


update : Client { a | key : Nav.Key } -> Msg -> Detail -> ( Detail, Cmd Msg )
update client msg (Detail params) =
    case msg of
        Fetched record ->
            ( Detail { params | record = Just record }, Cmd.none )

        DeleteModalOpened ->
            ( Detail { params | confirmDelete = True }, Cmd.none )

        DeleteModalClosed ->
            ( Detail { params | confirmDelete = False }, Cmd.none )

        DeleteConfirmed ->
            case params.record of
                Just record ->
                    ( Detail params
                    , Client.delete client params.table record
                        |> attemptWithError Failed (always Deleted)
                    )

                Nothing ->
                    ( Detail params, Cmd.none )

        Deleted ->
            ( Detail params
            , Notification.confirm "The record was deleted"
                |> navigate client.key params.table.name
            )

        NotificationChanged _ ->
            ( Detail params, Cmd.none )

        Failed _ ->
            ( Detail params, Cmd.none )


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



-- Http


fetch : Client a -> Detail -> Cmd Msg
fetch client (Detail { table, id }) =
    Client.fetch client table id
        |> attemptWithError Failed Fetched



-- View


view : Schema -> Detail -> Html Msg
view schema (Detail params) =
    case params.record of
        Just ({ tableName } as record) ->
            section
                [ class "record-detail" ]
                [ h1
                    []
                    [ Record.label record
                        |> Maybe.withDefault ""
                        |> (++) (String.humanize tableName ++ " - ")
                        |> text
                    ]
                , article
                    [ class "card" ]
                    [ table
                        []
                        (sortedFields record |> List.map (tableRow tableName))
                    , actions record
                    ]
                , if params.confirmDelete then
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

        Nothing ->
            text "loading"


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
                    [ href (Url.absolute [ record.tableName, id, "edit" ] [])
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
