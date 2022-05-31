module Internal.Client exposing
    ( Client
    , Msg
    , deleteRecord
    , fetchRecord
    , fetchRecordList
    , fetchSchema
    , getTable
    , init
    , isAuthSuccessMsg
    , isAuthenticated
    , post
    , saveRecord
    , schemaIsLoaded
    , selects
    , toHostUrl
    , toJwtString
    , toResponse
    , toSchema
    , update
    , view
    )

import Dict
import Dict.Extra as Dict
import Html exposing (Html, text)
import Http exposing (header)
import Internal.AuthScheme as AuthScheme exposing (AuthScheme)
import Internal.Field as Field
import Internal.Record as Record exposing (Record)
import Internal.Schema as Schema
    exposing
        ( Column
        , Constraint(..)
        , Schema
        , Table
        )
import Json.Decode as Decode exposing (Value)
import Json.Encode as Encode exposing (Value)
import Postgrest.Client as PG exposing (Endpoint, Request, Selectable)
import Task exposing (Task)
import Url exposing (Url)
import Utils.Task exposing (Error(..), handleResponse)


type Client
    = Client
        { host : Url
        , authScheme : AuthScheme
        , schema : Schema
        , response : Result Error Value
        }


type Msg
    = AuthChanged AuthScheme.Msg
    | SchemaFetched (Result Error Schema)


init : Url -> AuthScheme -> Client
init url authScheme =
    Client
        { host = url
        , authScheme = authScheme
        , schema = Dict.empty
        , response = Ok Encode.null
        }


toHostUrl : Client -> Url
toHostUrl (Client { host }) =
    host


toSchema : Client -> Schema
toSchema (Client { schema }) =
    schema


isAuthenticated : Client -> Bool
isAuthenticated (Client { authScheme }) =
    AuthScheme.isAuthenticated authScheme


isAuthSuccessMsg : Msg -> Bool
isAuthSuccessMsg msg =
    case msg of
        AuthChanged authMsg ->
            AuthScheme.isSuccessMsg authMsg

        SchemaFetched _ ->
            False


schemaIsLoaded : Client -> Bool
schemaIsLoaded (Client { schema }) =
    not (schema == Dict.empty)


toJwtString : Client -> Maybe String
toJwtString (Client { authScheme }) =
    AuthScheme.toJwt authScheme
        |> Maybe.map PG.jwtString


tablePrimaryKeyName : Table -> Maybe String
tablePrimaryKeyName table =
    Dict.find (\_ column -> Field.isPrimaryKey column) table.columns
        |> Maybe.map Tuple.first


getTable : String -> Client -> Maybe Table
getTable tableName (Client { schema }) =
    Dict.get tableName schema


toResponse : Client -> Result Error Value
toResponse (Client { response }) =
    response



-- UPDATE


update : Msg -> Client -> ( Client, Cmd Msg )
update msg (Client params) =
    case msg of
        AuthChanged childMsg ->
            let
                ( authScheme, cmd ) =
                    AuthScheme.update childMsg params.authScheme
            in
            ( Client { params | authScheme = authScheme }
            , Cmd.map AuthChanged cmd
            )

        SchemaFetched (Ok schema) ->
            ( Client { params | schema = schema }
            , Cmd.none
            )

        SchemaFetched (Err err) ->
            ( Client { params | response = Err err }
            , Cmd.none
            )


fetchSchema : Client -> Cmd Msg
fetchSchema (Client params) =
    Schema.fetchSchema params.host
        |> Task.attempt SchemaFetched



-- VIEW


view : Client -> Html Msg
view (Client params) =
    if AuthScheme.isAuthenticated params.authScheme then
        text ""

    else
        Html.map AuthChanged (AuthScheme.view params.authScheme)


fetchRecord : Client -> Table -> String -> Task Never Client
fetchRecord ((Client params) as client) table id =
    case tablePrimaryKeyName table of
        Just primaryKeyName ->
            resourceEndpoint params.host table
                |> PG.getOne
                |> PG.setParams
                    [ PG.select (selects table)
                    , PG.eq (PG.string id) |> PG.param primaryKeyName
                    ]
                |> requestToTask client

        Nothing ->
            missingPrimaryKey client


fetchRecordList : Client -> Table -> PG.Params -> Task Never Client
fetchRecordList ((Client params) as client) table pgParams =
    mapWithToken client
        (\token ->
            resourceEndpoint params.host table
                |> PG.getMany
                -- |> PG.setParams [ PG.select <| selects table ]
                |> PG.setParams pgParams
                |> PG.toTask token
                |> Task.map (Encode.list identity)
        )


saveRecord : Client -> Record -> Maybe String -> Task Never Client
saveRecord client record maybeId =
    case maybeId of
        Just _ ->
            updateRecord client record maybeId

        Nothing ->
            create client record


create : Client -> Record -> Task Never Client
create ((Client params) as client) record =
    Record.encode record
        |> PG.postOne (resourceEndpoint params.host record.table)
        |> PG.setParams [ PG.select <| selects record.table ]
        |> requestToTask client


updateRecord : Client -> Record -> Maybe String -> Task Never Client
updateRecord ((Client params) as client) record maybeId =
    case tablePrimaryKeyName record.table of
        Just primaryKeyName ->
            case maybeId of
                Just id ->
                    let
                        endpoint =
                            resourceEndpoint params.host record.table

                        primaryKey =
                            PG.primaryKey ( primaryKeyName, PG.string )
                    in
                    Record.encode record
                        |> PG.patchByPrimaryKey endpoint primaryKey id
                        |> PG.setParams [ PG.select <| selects record.table ]
                        |> requestToTask client

                Nothing ->
                    missingId client

        Nothing ->
            missingPrimaryKey client


deleteRecord : Client -> Record -> Task Never Client
deleteRecord ((Client params) as client) record =
    case tablePrimaryKeyName record.table of
        Just primaryKeyName ->
            case Record.id record of
                Just id ->
                    mapWithToken client
                        (\token ->
                            let
                                endpoint =
                                    resourceEndpoint params.host record.table

                                primaryKey =
                                    PG.primaryKey ( primaryKeyName, PG.string )
                            in
                            PG.deleteByPrimaryKey endpoint primaryKey id
                                |> PG.toTask token
                                |> Task.map (\_ -> Record.encode record)
                        )

                Nothing ->
                    missingId client

        Nothing ->
            missingPrimaryKey client


post : Client -> String -> Value -> Task Never Client
post (Client params) path value =
    let
        host =
            params.host
    in
    case toJwtString (Client params) of
        Just token ->
            Http.task
                { method = "POST"
                , headers =
                    [ header "Authorization" ("Bearer " ++ token)
                    , header "Prefer" "resolution=merge-duplicates"
                    ]
                , url = Url.toString { host | path = path }
                , body = Http.jsonBody value
                , resolver = Http.stringResolver (handleResponse (\_ -> Ok ()))
                , timeout = Nothing
                }
                |> Task.map (\_ -> Client { params | response = Ok Encode.null })
                |> Task.onError
                    (\err ->
                        Task.succeed (Client { params | response = Err err })
                    )

        Nothing ->
            authFailed (Client params)



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


resourceEndpoint : Url -> Table -> Endpoint Value
resourceEndpoint url table =
    PG.endpoint ({ url | path = "/" ++ table.name } |> Url.toString)
        Decode.value



-- TASK UTILS


requestToTask : Client -> Request Value -> Task Never Client
requestToTask client request =
    mapWithToken client (\token -> PG.toTask token request)


mapTask :
    Client
    -> (PG.JWT -> Task PG.Error Value)
    -> PG.JWT
    -> Task Never Client
mapTask ((Client params) as client) taskFn jwt =
    taskFn jwt
        |> Task.map (\value -> Client { params | response = Ok value })
        |> Task.onError
            (\err ->
                case err of
                    PG.BadStatus 401 _ _ ->
                        authFailed client

                    _ ->
                        Task.succeed
                            (Client
                                { params | response = Err (PGError err) }
                            )
            )


mapWithToken : Client -> (PG.JWT -> Task PG.Error Value) -> Task Never Client
mapWithToken ((Client params) as client) taskFn =
    AuthScheme.toJwt params.authScheme
        |> Maybe.map (mapTask client taskFn)
        |> Maybe.withDefault (authFailed client)


authFailed : Client -> Task Never Client
authFailed (Client params) =
    Task.succeed
        (Client
            { params
                | response = Err AuthError
                , authScheme = AuthScheme.fail params.authScheme
            }
        )


missingId : Client -> Task Never Client
missingId (Client params) =
    Task.succeed
        (Client
            { params | response = Err (RequestError "Record has no id") }
        )


missingPrimaryKey : Client -> Task Never Client
missingPrimaryKey (Client params) =
    Task.succeed
        (Client
            { params | response = Err (RequestError "No primary id in table") }
        )
