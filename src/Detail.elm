module Detail exposing (Detail, Msg, fetch, init, mapMsg, update, view)

import Dict
import Html
    exposing
        ( Html
        , a
        , article
        , div
        , h1
        , section
        , table
        , td
        , text
        , th
        , tr
        )
import Html.Attributes exposing (class, href)
import Postgrest.Client as PG
import Postgrest.Field as Field exposing (Field)
import Postgrest.Resource as Resource exposing (Resource)
import Postgrest.Resource.Client as Client exposing (Client)
import Postgrest.Schema exposing (Table)
import Postgrest.Value exposing (Value(..))
import PostgrestAdmin.AuthScheme as AuthScheme
import PostgrestAdmin.OuterMsg as OuterMsg exposing (OuterMsg)
import String.Extra as String
import Task
import Url.Builder as Url
import Utils.Task exposing (Error(..), attemptWithError, fail)


type Msg
    = Fetched Resource
    | Failed Error


type alias Params =
    { table : Table
    , resourcesName : String
    , fieldNames : List String
    , id : String
    }


type Detail
    = Detail Params (Maybe Resource)


init : Params -> Detail
init params =
    Detail params Nothing


update : Msg -> Detail -> ( Detail, Cmd Msg )
update msg (Detail params maybeResource) =
    case msg of
        Fetched resource ->
            ( Detail params (Just resource), Cmd.none )

        Failed _ ->
            ( Detail params maybeResource, Cmd.none )


mapMsg : Msg -> OuterMsg
mapMsg msg =
    case msg of
        Failed err ->
            OuterMsg.RequestFailed err

        _ ->
            OuterMsg.Pass



-- Http


fetch : Client a -> Detail -> Cmd Msg
fetch client (Detail { table, resourcesName, id } maybeResource) =
    case maybeResource of
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
view (Detail { resourcesName } maybeResource) =
    case maybeResource of
        Just resource ->
            section
                [ class "resource-detail" ]
                [ h1
                    []
                    [ Resource.label resource
                        |> Maybe.withDefault "New"
                        |> (++) (String.humanize resourcesName ++ " - ")
                        |> text
                    ]
                , article
                    [ class "card" ]
                    [ table
                        []
                        (sortedFields resource
                            |> List.map (tableRow resourcesName)
                        )
                    , actions resourcesName resource
                    ]
                ]

        Nothing ->
            text "loading"


actions : String -> Resource -> Html Msg
actions resourcesName resource =
    case Resource.id resource of
        Just id ->
            div
                []
                [ a
                    [ href (Url.absolute [ resourcesName, id, "edit" ] [])
                    , class "button"
                    ]
                    [ text "Edit" ]
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


sortedFields : Resource -> List ( String, Field )
sortedFields resource =
    Dict.toList resource
        |> List.sortWith Field.compareTuple
