module Internal.PageDetail exposing
    ( Msg
    , PageDetail
    , init
    , onLogin
    , update
    , view
    )

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
import Internal.Cmd as AppCmd
import Internal.Config exposing (DetailActions)
import Internal.Field as Field exposing (Field)
import Internal.Record as Record exposing (Record)
import Internal.Schema exposing (Constraint(..), Reference, Schema, Table)
import Internal.Value exposing (Value(..))
import PostgRestAdmin.Client as Client exposing (Client)
import PostgRestAdmin.Notification as Notification
import String.Extra as String
import Url
import Url.Builder as Url
import Utils.Task exposing (Error(..))


type Msg
    = LoggedIn Client
    | Fetched (Result Error Record)
    | Deleted (Result Error ())
    | DeleteModalOpened
    | DeleteModalClosed
    | DeleteConfirmed


type PageDetail
    = PageDetail
        { client : Client
        , key : Nav.Key
        , table : Table
        , id : String
        , record : Maybe Record
        , detailActions : DetailActions
        , confirmDelete : Bool
        }


init :
    { client : Client
    , table : Table
    , id : String
    , detailActions : DetailActions
    }
    -> Nav.Key
    -> ( PageDetail, AppCmd.Cmd Msg )
init { client, table, id, detailActions } key =
    let
        pageDetail =
            PageDetail
                { client = client
                , key = key
                , table = table
                , id = id
                , record = Nothing
                , detailActions = detailActions
                , confirmDelete = False
                }
    in
    ( pageDetail, fetch pageDetail )


fetch : PageDetail -> AppCmd.Cmd Msg
fetch (PageDetail { client, table, id }) =
    Client.fetchRecord
        { client = client
        , table = table
        , id = id
        , expect = Fetched
        }


onLogin : Client -> Msg
onLogin =
    LoggedIn


update : Msg -> PageDetail -> ( PageDetail, AppCmd.Cmd Msg )
update msg (PageDetail params) =
    case msg of
        LoggedIn client ->
            let
                pageDetail =
                    PageDetail { params | client = client }
            in
            ( pageDetail, fetch pageDetail )

        Fetched (Ok record) ->
            ( PageDetail { params | record = Just record }
            , AppCmd.none
            )

        Fetched (Err _) ->
            ( PageDetail params, AppCmd.none )

        Deleted (Ok _) ->
            ( PageDetail params
            , AppCmd.batch
                [ Url.absolute [ params.table.name ] []
                    |> Nav.pushUrl params.key
                    |> AppCmd.wrap
                , Notification.confirm "The record was deleted"
                ]
            )

        Deleted (Err _) ->
            ( PageDetail params, AppCmd.none )

        DeleteModalOpened ->
            ( PageDetail { params | confirmDelete = True }, AppCmd.none )

        DeleteModalClosed ->
            ( PageDetail { params | confirmDelete = False }, AppCmd.none )

        DeleteConfirmed ->
            ( PageDetail params
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


view : Schema -> PageDetail -> Html Msg
view schema (PageDetail params) =
    case params.record of
        Nothing ->
            text ""

        Just record ->
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
                    , actions params.detailActions record
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


actions : DetailActions -> Record -> Html Msg
actions customActions record =
    case Record.id record of
        Just id ->
            div
                [ class "actions" ]
                (List.map
                    (\( copy, url ) -> linkButton { url = url id, text = copy })
                    customActions
                    ++ [ linkButton
                            { url =
                                Url.absolute
                                    [ Record.tableName record, id, "edit" ]
                                    []
                            , text = "Edit"
                            }
                       , button
                            [ onClick DeleteModalOpened
                            , class "button button-danger"
                            ]
                            [ text "Delete" ]
                       ]
                )

        Nothing ->
            text ""


linkButton : { url : String, text : String } -> Html Msg
linkButton params =
    a
        [ href params.url, class "button" ]
        [ text params.text ]


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
