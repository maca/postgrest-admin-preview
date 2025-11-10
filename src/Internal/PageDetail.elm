module Internal.PageDetail exposing
    ( Msg
    , PageDetail
    , init
    , onLogin
    , update
    , view
    )

import Browser.Navigation as Nav
import Dict exposing (Dict)
import Html
    exposing
        ( Html
        , a
        , article
        , aside
        , button
        , div
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
import Http exposing (header)
import Internal.Cmd as AppCmd
import Internal.Config exposing (DetailActions)
import Internal.Field as Field exposing (Field)
import Internal.Record as Record exposing (Record)
import Internal.Schema exposing (Reference, Table)
import Json.Decode as Decode
import PostgRestAdmin.Client as Client exposing (Client, Collection, Error)
import PostgRestAdmin.MountPath exposing (MountPath, breadcrumbs, path)
import PostgRestAdmin.Notification as Notification
import String.Extra as String
import Url.Builder as Url exposing (QueryParameter, string)


type Msg
    = LoggedIn Client
    | Fetched (Result Error Record)
    | GotCount String (Result Error (Collection ()))
    | Deleted (Result Error ())
    | DeleteModalOpened
    | DeleteModalClosed
    | DeleteConfirmed


type alias PageDetail =
    { client : Client
    , mountPath : MountPath
    , key : Nav.Key
    , table : Table
    , id : String
    , record : Maybe Record
    , detailActions : DetailActions
    , confirmDelete : Bool
    , counts : Dict String Int
    }


init :
    { client : Client
    , mountPath : MountPath
    , table : Table
    , id : String
    , detailActions : DetailActions
    }
    -> Nav.Key
    -> ( PageDetail, AppCmd.Cmd Msg )
init { client, mountPath, table, id, detailActions } key =
    let
        pageDetail =
            { client = client
            , mountPath = mountPath
            , key = key
            , table = table
            , id = id
            , record = Nothing
            , detailActions = detailActions
            , confirmDelete = False
            , counts = Dict.empty
            }
    in
    ( pageDetail
    , fetch pageDetail
    )


fetch : PageDetail -> AppCmd.Cmd Msg
fetch { client, table, id } =
    Client.fetchRecord
        { client = client
        , table = table
        , id = id
        , decoder = Record.decoder table
        , expect = Fetched
        }


onLogin : Client -> Msg
onLogin =
    LoggedIn


update : Msg -> PageDetail -> ( PageDetail, AppCmd.Cmd Msg )
update msg params =
    case msg of
        LoggedIn client ->
            let
                pageDetail =
                    { params | client = client }
            in
            ( pageDetail, fetch pageDetail )

        Fetched (Ok record) ->
            ( { params | record = Just record }
            , references params.client record
                |> List.map
                    (\ref ->
                        Client.requestMany
                            { client = params.client
                            , method = "HEAD"
                            , headers = [ header "Prefer" "count=exact" ]
                            , path = referencePath ref []
                            , body = Http.emptyBody
                            , decoder = Decode.succeed ()
                            , expect = GotCount (Client.tableName ref.table)
                            }
                    )
                |> AppCmd.batch
            )

        GotCount tableName (Ok { total }) ->
            ( { params
                | counts = Dict.insert tableName total params.counts
              }
            , AppCmd.none
            )

        GotCount _ _ ->
            ( params, AppCmd.none )

        Fetched (Err err) ->
            ( params
            , Notification.error (Client.errorToString err)
            )

        Deleted (Ok _) ->
            ( params
            , AppCmd.batch
                [ Url.absolute [ params.table.name ] []
                    |> Nav.pushUrl params.key
                    |> AppCmd.wrap
                , Notification.confirm "The record was deleted"
                ]
            )

        Deleted (Err err) ->
            ( params
            , Notification.error (Client.errorToString err)
            )

        DeleteModalOpened ->
            ( { params | confirmDelete = True }, AppCmd.none )

        DeleteModalClosed ->
            ( { params | confirmDelete = False }, AppCmd.none )

        DeleteConfirmed ->
            ( params
            , case params.record of
                Just record ->
                    Client.deleteRecord
                        { record = record
                        , expect = Deleted
                        }
                        params.client

                Nothing ->
                    AppCmd.none
            )



-- View


view : PageDetail -> Html Msg
view params =
    case params.record of
        Nothing ->
            text ""

        Just record ->
            let
                tableName =
                    params.table.name
            in
            section
                [ class "record-detail" ]
                [ breadcrumbs params.mountPath
                    tableName
                    [ ( tableName, Nothing )
                    , ( params.id, Record.label record )
                    ]
                , h2
                    []
                    [ Record.label record
                        |> Maybe.map text
                        |> Maybe.withDefault (text "")
                    ]
                , article
                    [ class "card" ]
                    [ table
                        []
                        (sortedFields record
                            |> List.map
                                (tableRow params.mountPath
                                    (Record.tableName record)
                                )
                        )
                    , actions params record
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
                    (references params.client record
                        |> List.map
                            (referenceToHtml params record)
                    )
                ]


referenceToHtml : PageDetail -> Record -> Reference -> Html Msg
referenceToHtml { mountPath, counts } record { table, foreignKeyValue } =
    a
        [ class "card association"
        , href
            (path mountPath <|
                Url.absolute [ record.table.name, foreignKeyValue, table.name ] []
            )
        ]
        [ text (String.humanize table.name)
        , Dict.get table.name counts
            |> Maybe.map (\i -> " (" ++ String.fromInt i ++ ")")
            |> Maybe.withDefault ""
            |> text
        ]


actions : PageDetail -> Record -> Html Msg
actions { mountPath, detailActions } record =
    case Record.id record of
        Just id ->
            div
                [ class "actions" ]
                (List.map
                    (\( copy, buildUrl ) ->
                        a
                            [ href (path mountPath (buildUrl record id))
                            , class "button"
                            ]
                            [ text copy ]
                    )
                    detailActions
                    ++ [ a
                            [ href
                                (path mountPath <|
                                    Url.absolute
                                        [ Record.tableName record, id, "edit" ]
                                        []
                                )
                            , class "button"
                            ]
                            [ text "Edit" ]
                       , button
                            [ onClick DeleteModalOpened
                            , class "button button-danger"
                            ]
                            [ text "Delete" ]
                       ]
                )

        Nothing ->
            text ""


tableRow : MountPath -> String -> ( String, Field ) -> Html Msg
tableRow mountPath resourcesName ( name, field ) =
    tr
        []
        [ th [] [ text (String.humanize name) ]
        , td [] [ Field.toHtml mountPath resourcesName field ]
        ]



-- UTILS


references : Client -> Record -> List Reference
references client record =
    Record.referencedBy client.schema record


referencePath : Reference -> List QueryParameter -> String
referencePath { foreignKeyName, foreignKeyValue, table } query =
    Url.absolute
        [ table.name ]
        (string foreignKeyName ("eq." ++ foreignKeyValue) :: query)


sortedFields : Record -> List ( String, Field )
sortedFields record =
    Dict.toList record.fields
        |> List.sortWith Field.compareTuple
