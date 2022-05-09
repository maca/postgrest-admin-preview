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
import Postgrest.Client as PG
import Postgrest.Field as Field exposing (Field)
import Postgrest.Record as Record exposing (Record)
import Postgrest.Record.Client as Client exposing (Client)
import Postgrest.Schema exposing (Table)
import Postgrest.Value exposing (Value(..))
import PostgrestAdmin.AuthScheme as AuthScheme
import PostgrestAdmin.OuterMsg as OuterMsg exposing (OuterMsg)
import String.Extra as String
import Task
import Url.Builder as Url
import Utils.Task exposing (Error(..), attemptWithError, fail)


type Msg
    = Fetched Record
    | Deleted
    | DeleteModalOpened
    | DeleteModalClosed
    | Failed Error


type alias Params a =
    { a
        | table : Table
        , resourcesName : String
        , fieldNames : List String
        , id : String
    }


type Detail
    = Detail
        (Params
            { confirmDelete : Bool
            , resource : Maybe Record
            }
        )


init : Params {} -> Detail
init params =
    Detail
        { table = params.table
        , resourcesName = params.resourcesName
        , fieldNames = params.fieldNames
        , id = params.id
        , confirmDelete = False
        , resource = Nothing
        }


update : Msg -> Detail -> ( Detail, Cmd Msg )
update msg (Detail params) =
    case msg of
        Fetched resource ->
            ( Detail { params | resource = Just resource }, Cmd.none )

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
fetch client (Detail { table, resourcesName, id, resource }) =
    case resource of
        Just _ ->
            Cmd.none

        Nothing ->
            case AuthScheme.toJwt client.authScheme of
                Just token ->
                    Client.fetchOne client table resourcesName id
                        |> PG.toTask token
                        |> Task.mapError PGError
                        |> attemptWithError Failed Fetched

                Nothing ->
                    fail Failed AuthError



-- View


view : Detail -> Html Msg
view (Detail params) =
    case params.resource of
        Just resource ->
            section
                [ class "resource-detail" ]
                [ h1
                    []
                    [ Record.label resource
                        |> Maybe.withDefault ""
                        |> (++) (String.humanize params.resourcesName ++ " - ")
                        |> text
                    ]
                , article
                    [ class "card" ]
                    [ table
                        []
                        (sortedFields resource
                            |> List.map (tableRow params.resourcesName)
                        )
                    , actions params.resourcesName resource
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


actions : String -> Record -> Html Msg
actions resourcesName resource =
    case Record.id resource of
        Just id ->
            div
                [ class "action" ]
                [ a
                    [ href (Url.absolute [ resourcesName, id, "edit" ] [])
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
    tr []
        [ th
            []
            [ text (String.humanize name) ]
        , td
            []
            [ Field.toHtml (always (class "")) resourcesName field ]
        ]



-- Utils


sortedFields : Record -> List ( String, Field )
sortedFields resource =
    Dict.toList resource
        |> List.sortWith Field.compareTuple
