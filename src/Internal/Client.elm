module Internal.Client exposing
    ( Client
    , Msg
    , authFailed
    , fetchSchema
    , getTable
    , init
    , isAuthSuccessMsg
    , isAuthenticated
    , schemaIsLoaded
    , selects
    , task
    , toHostUrl
    , toJwtString
    , toSchema
    , update
    , view
    )

import Dict
import Dict.Extra as Dict
import Html exposing (Html, text)
import Http exposing (header)
import Internal.AuthScheme as AuthScheme exposing (AuthScheme)
import Internal.Schema as Schema
    exposing
        ( Column
        , Constraint(..)
        , Schema
        , Table
        )
import Json.Encode as Encode exposing (Value)
import Postgrest.Client as PG exposing (Selectable)
import Task exposing (Task)
import Url exposing (Url)
import Utils.Task exposing (Error(..), handleJsonResponse)


type alias Client =
    { host : Url
    , authScheme : AuthScheme
    , schema : Schema
    }


type Msg
    = AuthChanged AuthScheme.Msg
    | SchemaFetched (Result Error Schema)


init : Url -> AuthScheme -> Client
init url authScheme =
    { host = url
    , authScheme = authScheme
    , schema = Dict.empty
    }


toHostUrl : Client -> Url
toHostUrl { host } =
    host


toSchema : Client -> Schema
toSchema { schema } =
    schema


isAuthenticated : Client -> Bool
isAuthenticated { authScheme } =
    AuthScheme.isAuthenticated authScheme


isAuthSuccessMsg : Msg -> Bool
isAuthSuccessMsg msg =
    case msg of
        AuthChanged authMsg ->
            AuthScheme.isSuccessMsg authMsg

        SchemaFetched _ ->
            False


authFailed : Client -> Client
authFailed client =
    { client | authScheme = AuthScheme.fail client.authScheme }


schemaIsLoaded : Client -> Bool
schemaIsLoaded { schema } =
    not (schema == Dict.empty)


toJwtString : Client -> Maybe String
toJwtString { authScheme } =
    AuthScheme.toJwt authScheme
        |> Maybe.map PG.jwtString


getTable : String -> Client -> Maybe Table
getTable tableName { schema } =
    Dict.get tableName schema



-- UPDATE


update : Msg -> Client -> ( Client, Cmd Msg )
update msg client =
    case msg of
        AuthChanged childMsg ->
            let
                ( authScheme, cmd ) =
                    AuthScheme.update childMsg client.authScheme
            in
            ( { client | authScheme = authScheme }
            , Cmd.map AuthChanged cmd
            )

        SchemaFetched (Ok schema) ->
            ( { client | schema = schema }
            , Cmd.none
            )

        SchemaFetched (Err err) ->
            ( client
            , Cmd.none
            )


fetchSchema : Client -> Cmd Msg
fetchSchema client =
    task
        { client = client
        , method = "GET"
        , headers = []
        , path = "/"
        , body = Http.emptyBody
        , resolver = Http.stringResolver (handleJsonResponse Schema.decoder)
        , timeout = Nothing
        }
        |> Task.attempt SchemaFetched



-- VIEW


view : Client -> Html Msg
view { authScheme } =
    if AuthScheme.isAuthenticated authScheme then
        text ""

    else
        Html.map AuthChanged (AuthScheme.view authScheme)


task :
    { client : Client
    , method : String
    , headers : List Http.Header
    , path : String
    , body : Http.Body
    , resolver : Http.Resolver Error a
    , timeout : Maybe Float
    }
    -> Task Error a
task { client, method, headers, path, body, resolver, timeout } =
    let
        host =
            client.host
    in
    case toJwtString client of
        Just jwt ->
            Http.task
                { method = method
                , headers =
                    header "Authorization" ("Bearer " ++ jwt) :: headers
                , url = Url.toString { host | path = path }
                , body = body
                , resolver = resolver
                , timeout = timeout
                }
                |> Task.onError fail

        Nothing ->
            fail AuthError


fail : Error -> Task Error a
fail err =
    case err of
        AuthError ->
            Task.fail AuthError

        _ ->
            Task.fail err



-- HTTP UTILS


selects : Table -> List Selectable
selects table =
    Dict.values table.columns
        |> List.filterMap associationJoin
        |> (++) (Dict.keys table.columns |> List.map PG.attribute)


associationJoin : Column -> Maybe Selectable
associationJoin { constraint } =
    case constraint of
        ForeignKey { tableName, labelColumnName } ->
            labelColumnName
                |> Maybe.map
                    (\n -> PG.resource tableName (PG.attributes [ n, "id" ]))

        _ ->
            Nothing
