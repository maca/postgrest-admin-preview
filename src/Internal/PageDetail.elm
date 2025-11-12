module Internal.PageDetail exposing
    ( Model
    , Msg
    , init
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
import Internal.Schema exposing (Record, Reference, Table)
import Internal.Value
import Json.Decode as Decode
import PostgRestAdmin.Client as Client exposing (Client, Count, Error)
import PostgRestAdmin.MountPath exposing (MountPath, breadcrumbs, path)
import PostgRestAdmin.Notification as Notification
import String.Extra as String
import Time.Extra as Time
import Url.Builder as Url


type Msg
    = Fetched (Result Error Record)
    | GotCount String (Result Error ( List (), Count ))
    | Deleted (Result Error ())
    | DeleteModalOpened
    | DeleteModalClosed
    | DeleteConfirmed


type alias Model =
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
    -> ( Model, AppCmd.Cmd Msg )
init { client, mountPath, table, id, detailActions } key =
    ( { client = client
      , mountPath = mountPath
      , key = key
      , table = table
      , id = id
      , record = Nothing
      , detailActions = detailActions
      , confirmDelete = False
      , counts = Dict.empty
      }
    , Client.fetchRecord
        { client = client
        , table = table
        , id = id
        , decoder = Internal.Schema.recordDecoder table
        , expect = Fetched
        }
    )


update : Msg -> Model -> ( Model, AppCmd.Cmd Msg )
update msg model =
    case msg of
        Fetched (Ok record) ->
            ( { model | record = Just record }
            , model.table.referencedBy
                |> List.map
                    (\ref ->
                        Client.requestMany
                            { client = model.client
                            , method = "HEAD"
                            , headers = [ header "Prefer" "count=exact" ]
                            , path =
                                Url.absolute
                                    [ ref.tableName ]
                                    [ Url.string ref.foreignKeyName ("eq." ++ model.id) ]
                            , body = Http.emptyBody
                            , decoder = Decode.succeed ()
                            , expect = GotCount ref.tableName
                            }
                    )
                |> AppCmd.batch
            )

        GotCount tableName (Ok ( _, count )) ->
            ( { model
                | counts = Dict.insert tableName count.total model.counts
              }
            , AppCmd.none
            )

        GotCount _ _ ->
            ( model, AppCmd.none )

        Fetched (Err err) ->
            ( model
            , Notification.error (Client.errorToString err)
            )

        Deleted (Ok _) ->
            ( model
            , AppCmd.batch
                [ Url.absolute [ model.table.name ] []
                    |> Nav.pushUrl model.key
                    |> AppCmd.wrap
                , Notification.confirm "The record was deleted"
                ]
            )

        Deleted (Err err) ->
            ( model
            , Notification.error (Client.errorToString err)
            )

        DeleteModalOpened ->
            ( { model | confirmDelete = True }, AppCmd.none )

        DeleteModalClosed ->
            ( { model | confirmDelete = False }, AppCmd.none )

        DeleteConfirmed ->
            ( model
            , Client.deleteRecord
                { client = model.client
                , table = model.table
                , id = model.id
                , expect = Deleted
                }
            )



-- View


view : Model -> Html Msg
view model =
    case model.record of
        Nothing ->
            Html.text ""

        Just record ->
            let
                tableName =
                    model.table.name
            in
            Html.section
                [ Attrs.class "record-detail" ]
                [ breadcrumbs model.mountPath
                    tableName
                    [ ( tableName, Nothing )
                    , ( model.id, recordLabel model.table record )
                    ]
                , Html.h2
                    []
                    [ recordLabel model.table record
                        |> Maybe.map Html.text
                        |> Maybe.withDefault (Html.text "")
                    ]
                , Html.article
                    [ Attrs.class "card" ]
                    [ Html.table
                        []
                        (sortedFields model.table record
                            |> List.map tableRow
                        )
                    , actions model
                    ]
                , if model.confirmDelete then
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
                    (model.table.referencedBy
                        |> List.map
                            (referenceToHtml model)
                    )
                ]


referenceToHtml : Model -> Reference -> Html Msg
referenceToHtml params ref =
    Html.a
        [ Attrs.class "card association"
        , Attrs.href
            (path params.mountPath <|
                Url.absolute [ params.table.name, params.id, ref.tableName ] []
            )
        ]
        [ Html.text (String.humanize ref.tableName)
        , Dict.get ref.tableName params.counts
            |> Maybe.map (\i -> " (" ++ String.fromInt i ++ ")")
            |> Maybe.withDefault ""
            |> Html.text
        ]


actions : Model -> Html Msg
actions { mountPath, table, id } =
    Html.div
        [ Attrs.class "actions" ]
        [ Html.a
            [ Attrs.href
                (path mountPath <|
                    Url.absolute
                        [ table.name, id, "edit" ]
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


tableRow : ( String, Internal.Value.Value ) -> Html Msg
tableRow ( name, value ) =
    Html.tr
        []
        [ Html.th [] [ Html.text (String.humanize name) ]
        , Html.td [] [ valueToHtml value ]
        ]


valueToHtml : Internal.Value.Value -> Html msg
valueToHtml value =
    case value of
        Internal.Value.PFloat maybe ->
            maybeToHtml String.fromFloat maybe

        Internal.Value.PInt maybe ->
            maybeToHtml String.fromInt maybe

        Internal.Value.PString maybe ->
            maybeToHtml identity maybe

        Internal.Value.PBool maybe ->
            maybeToHtml
                (\bool ->
                    if bool then
                        "true"

                    else
                        "false"
                )
                maybe

        Internal.Value.PTime maybe ->
            maybeToHtml Time.format maybe

        Internal.Value.PDate maybe ->
            maybeToHtml Time.toDateString maybe

        Internal.Value.PText maybe ->
            maybeToHtml identity maybe

        Internal.Value.PJson _ ->
            Html.pre
                []
                [ Internal.Value.toString value
                    |> maybeToHtml identity
                ]

        Internal.Value.Unknown _ ->
            Html.text "?"


maybeToHtml : (a -> String) -> Maybe a -> Html msg
maybeToHtml func maybe =
    Maybe.map func maybe |> Maybe.withDefault "" |> Html.text



-- UTILS


sortedFields : Table -> Record -> List ( String, Internal.Value.Value )
sortedFields table record =
    Dict.toList record
        |> List.sortBy
            (\( name, _ ) ->
                case Dict.get name table.columns of
                    Just column ->
                        if column.constraint == Internal.Schema.PrimaryKey then
                            0

                        else
                            case column.constraint of
                                Internal.Schema.ForeignKey _ ->
                                    1

                                _ ->
                                    2

                    Nothing ->
                        3
            )


recordLabel : Table -> Record -> Maybe String
recordLabel table record =
    Internal.Schema.label table
        |> Maybe.andThen
            (\fieldName ->
                Dict.get fieldName record
                    |> Maybe.andThen Internal.Value.toString
            )
