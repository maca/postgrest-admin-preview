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
import Html exposing (Html)
import Html.Attributes as Attrs
import Html.Events as Events
import Http exposing (header)
import Internal.Cmd as AppCmd
import Internal.Config exposing (DetailActions)
import Internal.Field as Field exposing (Field)
import Internal.Record as Record exposing (Record)
import Internal.Schema exposing (Reference, Table)
import Json.Decode as Decode
import PostgRestAdmin.Client as Client exposing (Client, Count, Error)
import PostgRestAdmin.MountPath exposing (MountPath, breadcrumbs, path)
import PostgRestAdmin.Notification as Notification
import String.Extra as String
import Url.Builder as Url exposing (QueryParameter, string)


type Msg
    = LoggedIn Client
    | Fetched (Result Error Record)
    | GotCount String (Result Error ( List (), Count ))
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
                            , expect = GotCount ref.table.name
                            }
                    )
                |> AppCmd.batch
            )

        GotCount tableName (Ok ( _, count )) ->
            ( { params
                | counts = Dict.insert tableName count.total params.counts
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
            , Client.deleteRecord
                { client = params.client
                , table = params.table
                , id = params.id
                , expect = Deleted
                }
            )



-- View


view : PageDetail -> Html Msg
view params =
    case params.record of
        Nothing ->
            Html.text ""

        Just record ->
            let
                tableName =
                    params.table.name
            in
            Html.section
                [ Attrs.class "record-detail" ]
                [ breadcrumbs params.mountPath
                    tableName
                    [ ( tableName, Nothing )
                    , ( params.id, Record.label record )
                    ]
                , Html.h2
                    []
                    [ Record.label record
                        |> Maybe.map Html.text
                        |> Maybe.withDefault (Html.text "")
                    ]
                , Html.article
                    [ Attrs.class "card" ]
                    [ Html.table
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
                    Html.div
                        [ Attrs.class "modal-background" ]
                        [ Html.div
                            [ Attrs.class "modal-dialog" ]
                            [ Html.h2 []
                                [ Html.text """Are you sure you want to delete the
                                          record?"""
                                ]
                            , Html.p [] [ Html.text "This action cannot be undone." ]
                            , Html.div
                                [ Attrs.class "actions" ]
                                [ Html.button
                                    [ Attrs.class "button button-danger"
                                    , Events.onClick DeleteConfirmed
                                    ]
                                    [ Html.text "Delete" ]
                                , Html.button
                                    [ Attrs.class "button"
                                    , Events.onClick DeleteModalClosed
                                    ]
                                    [ Html.text "Cancel" ]
                                ]
                            ]
                        ]

                  else
                    Html.text ""
                , Html.aside
                    [ Attrs.class "associations" ]
                    (references params.client record
                        |> List.map
                            (referenceToHtml params record)
                    )
                ]


referenceToHtml : PageDetail -> Record -> Reference -> Html Msg
referenceToHtml { mountPath, counts } record { table, foreignKeyValue } =
    Html.a
        [ Attrs.class "card association"
        , Attrs.href
            (path mountPath <|
                Url.absolute [ record.table.name, foreignKeyValue, table.name ] []
            )
        ]
        [ Html.text (String.humanize table.name)
        , Dict.get table.name counts
            |> Maybe.map (\i -> " (" ++ String.fromInt i ++ ")")
            |> Maybe.withDefault ""
            |> Html.text
        ]


actions : PageDetail -> Record -> Html Msg
actions { mountPath, detailActions } record =
    case Record.id record of
        Just id ->
            Html.div
                [ Attrs.class "actions" ]
                (List.map
                    (\( copy, buildUrl ) ->
                        Html.a
                            [ Attrs.href (path mountPath (buildUrl record id))
                            , Attrs.class "button"
                            ]
                            [ Html.text copy ]
                    )
                    detailActions
                    ++ [ Html.a
                            [ Attrs.href
                                (path mountPath <|
                                    Url.absolute
                                        [ Record.tableName record, id, "edit" ]
                                        []
                                )
                            , Attrs.class "button"
                            ]
                            [ Html.text "Edit" ]
                       , Html.button
                            [ Events.onClick DeleteModalOpened
                            , Attrs.class "button button-danger"
                            ]
                            [ Html.text "Delete" ]
                       ]
                )

        Nothing ->
            Html.text ""


tableRow : MountPath -> String -> ( String, Field ) -> Html Msg
tableRow mountPath resourcesName ( name, field ) =
    Html.tr
        []
        [ Html.th [] [ Html.text (String.humanize name) ]
        , Html.td [] [ Field.toHtml mountPath resourcesName field ]
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
