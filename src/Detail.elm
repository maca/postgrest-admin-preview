module Detail exposing (Detail, Msg, fetch, init, mapMsg, update, view)

import Dict
import Html
    exposing
        ( Html
        , a
        , article
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
import Postgrest.Field as Field exposing (Field)
import Postgrest.Record as Record exposing (Record)
import Postgrest.Record.Client as Client exposing (Client)
import Postgrest.Schema exposing (Table)
import Postgrest.Value exposing (Value(..))
import PostgrestAdmin.OuterMsg as OuterMsg exposing (OuterMsg)
import String.Extra as String
import Url.Builder as Url
import Utils.Task exposing (Error(..), attemptWithError)


type Msg
    = Fetched Record
    | Deleted
    | DeleteModalOpened
    | DeleteModalClosed
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


update : Msg -> Detail -> ( Detail, Cmd Msg )
update msg (Detail params) =
    case msg of
        Fetched record ->
            ( Detail { params | record = Just record }, Cmd.none )

        DeleteModalOpened ->
            ( Detail { params | confirmDelete = True }, Cmd.none )

        DeleteModalClosed ->
            ( Detail { params | confirmDelete = False }, Cmd.none )

        Deleted ->
            ( Detail params, Cmd.none )

        Failed _ ->
            ( Detail params, Cmd.none )


mapMsg : Msg -> OuterMsg
mapMsg msg =
    case msg of
        Failed err ->
            OuterMsg.RequestFailed err

        _ ->
            OuterMsg.Pass



-- Http


fetch : Client a -> Detail -> Cmd Msg
fetch client (Detail { table, id }) =
    Client.fetch client table id
        |> attemptWithError Failed Fetched


view : Detail -> Html Msg
view (Detail params) =
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
                                    , onClick Deleted
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
                ]

        Nothing ->
            text "loading"


actions : Record -> Html Msg
actions record =
    case Record.id record of
        Just id ->
            div
                [ class "action" ]
                [ a
                    [ href (Url.absolute [ record.tableName, id, "edit" ] [])
                    , class "button"
                    ]
                    [ text "Edit" ]
                , button
                    [ onClick DeleteModalOpened
                    , class "button"
                    , class "button-danger"
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
        , td [] [ Field.toHtml (always (class "")) resourcesName field ]
        ]



-- Utils


sortedFields : Record -> List ( String, Field )
sortedFields record =
    Dict.toList record.fields
        |> List.sortWith Field.compareTuple