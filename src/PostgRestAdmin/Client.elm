module PostgRestAdmin.Client exposing
    ( Client
    , toHostUrl
    , Table
    , getTable
    , fetchRecord
    , fetchRecordList
    , saveRecord
    , deleteRecord
    , Error
    , errorToString
    , isAuthenticated
    , toJwtString
    , request, resolveWhatever
    )

{-|


# Client

@docs Client
@docs toHostUrl


# Table

@docs Table
@docs getTable
@docs tableName


# Requests

Note that the request functions **do not produce a vanilla Elm
[Cmd](https://package.elm-lang.org/packages/elm/core/latest/Platform-Cmd#Cmd)**
but a [PostgRestAdmin.Cmd](PostgRestAdmin.Cmd).

@docs fetchRecord
@docs fetchRecordList
@docs saveRecord
@docs deleteRecord
@docs post

@docs Error
@docs errorToString


# Authentication

@docs isAuthenticated
@docs toJwtString

-}

import Dict
import Dict.Extra as Dict
import Http exposing (header)
import Internal.Client as Client exposing (Client)
import Internal.Cmd as Internal
import Internal.Field as Field
import Internal.Schema as Schema exposing (Column, Constraint(..), Table)
import Json.Decode as Decode exposing (Decoder, Value)
import Json.Encode as Encode
import PostgRestAdmin.Cmd as AppCmd
import PostgRestAdmin.Record as Record exposing (Record)
import Postgrest.Client as PG exposing (Selectable)
import Task exposing (Task)
import Url exposing (Url)
import Utils.Task as Internal
    exposing
        ( Error(..)
        , handleJsonResponse
        , handleResponse
        )


{-| Represents a client for a PostgREST instance, including authentication
params.

See [Config](PostgRestAdmin.Config) and
[Config.FormAuth](PostgRestAdmin.Config.FormAuth) for authentication
configuration options.

-}
type alias Client =
    Client.Client


{-| Represents a PostgREST table.
-}
type alias Table =
    Schema.Table


{-| Request error.
-}
type alias Error =
    Internal.Error


{-| Obtain the PostgREST instance url.
-}
toHostUrl : Client -> Url
toHostUrl =
    Client.toHostUrl


{-| Does the client has a valid JWT?
-}
isAuthenticated : Client -> Bool
isAuthenticated =
    Client.isAuthenticated


{-| Obtain the JWT as a string.
-}
toJwtString : Client -> Maybe String
toJwtString client =
    Client.toJwtString client


{-| Obtain a table from the table name.
-}
getTable : String -> Client -> Maybe Table
getTable =
    Client.getTable


{-| Obtain the name of a table
-}
tableName : Table -> String
tableName table =
    table.name


{-| Transform [Error](#Error) to an error explanation.
-}
errorToString : Error -> String
errorToString =
    Internal.errorToString



-- VIEW


{-| Fetches a record for a given table.
`expect` param requires a function that returns a `Msg`.

You can use [expectRecord](#expectRecord) to interpret the result as a
[Record](PostgRestAdmin.Record).

    import PostgRestAdmin.Cmd as AppCmd

    fetch : (Result Error Record -> msg) -> String -> Client -> AppCmd.Cmd Msg
    fetch tagger tableName client =
        case getTable tableName client of
            Just table ->
                fetchRecord
                    { client = client
                    , table = table
                    , params = []
                    , expect = tagger
                    }

            Nothing ->
                AppCmd.none

-}
fetchRecord :
    { client : Client
    , table : Table
    , id : String
    , expect : Result Error Record -> msg
    }
    -> AppCmd.Cmd msg
fetchRecord { client, table, expect, id } =
    let
        mapper =
            mapResult expect (Record.decoder table)
    in
    case tablePrimaryKeyName table of
        Just primaryKeyName ->
            let
                queryString =
                    PG.toQueryString
                        [ PG.select (selects table)
                        , PG.eq (PG.string id) |> PG.param primaryKeyName
                        , PG.limit 1
                        ]
            in
            request
                { client = client
                , method = "GET"
                , headers =
                    [ header "Accept" "application/vnd.pgrst.object+json" ]
                , path = "/" ++ tableName table ++ "?" ++ queryString
                , body = Http.emptyBody
                , timeout = Nothing
                , resolver =
                    Http.stringResolver (handleJsonResponse Decode.value)
                , expect = mapper
                }

        Nothing ->
            Internal.Fetch mapper missingPrimaryKey


{-| Fetches a list of records for a given table.
`expect` param requires a function that returns a `Msg`.

You can use [expectRecordList](#expectRecordList) to interpret the result as a
list of [Record](PostgRestAdmin.Record)s.

    import PostgRestAdmin.Cmd as AppCmd

    fetchList : (Result Error (List Record) -> Msg) -> String -> Client -> AppCmd.Cmd Msg
    fetchList tagger tableName client =
        case getTable tableName client of
            Just table ->
                fetchRecordList
                    { client = client
                    , table = table
                    , params = []
                    , expect = Client.expectRecordList tagger table
                    }

            Nothing ->
                AppCmd.none

-}
fetchRecordList :
    { client : Client
    , table : Table
    , params : PG.Params
    , expect : Result Error (List Record) -> msg
    }
    -> AppCmd.Cmd msg
fetchRecordList { client, table, params, expect } =
    let
        queryString =
            PG.toQueryString
                (PG.select (selects table) :: params)
    in
    request
        { client = client
        , method = "GET"
        , headers = []
        , path = "/" ++ tableName table ++ "?" ++ queryString
        , body = Http.emptyBody
        , timeout = Nothing
        , resolver =
            Http.stringResolver
                (handleJsonResponse Decode.value)
        , expect = mapResult expect (Decode.list (Record.decoder table))
        }


{-| Saves a record.
`expect` param requires a function that returns a `Msg`.

You can use [expectRecord](#expectRecord) to interpret the result as a
[Record](PostgRestAdmin.Record).

    import PostgRestAdmin.Cmd as AppCmd

    save : (Result Error Record -> Msg) -> Record -> Maybe String -> Client -> AppCmd.Cmd Msg
    save tagger record id client =
        saveRecord
            { client = client
            , record = record
            , id = id
            , expect =
                Client.expectRecord tagger (Record.getTable record)
            }

-}
saveRecord :
    { client : Client
    , record : Record
    , id : Maybe String
    , expect : Result Error () -> msg
    }
    -> AppCmd.Cmd msg
saveRecord { client, record, id, expect } =
    let
        queryString =
            PG.toQueryString
                [ PG.select (selects record.table)
                , PG.limit 1
                ]

        path =
            Record.location record
                |> Maybe.map (\p -> p ++ "&" ++ queryString)
                |> Maybe.withDefault
                    ("/" ++ Record.tableName record ++ "?" ++ queryString)

        params =
            { client = client
            , method = "PATCH"
            , headers = []
            , path = path
            , body = Http.jsonBody (Record.encode record)
            , timeout = Nothing
            , resolver = resolveWhatever
            , expect = mapResult expect (Decode.succeed ())
            }
    in
    case id of
        Just _ ->
            request params

        Nothing ->
            request { params | method = "POST" }


{-| Deletes a record.
`expect` param requires a function that returns a `Msg`.

You can use [expectRecord](#expectRecord) to interpret the result as a
[Record](PostgRestAdmin.Record).

    import PostgRestAdmin.Cmd as AppCmd

    delete : (Result Error Record -> Msg) -> Record -> Client -> AppCmd.Cmd Msg
    delete tagger record client =
        deleteRecord
            { client = client
            , record = record
            , expect =
                Client.expectRecord tagger
                    (Record.getTable record)
            }

-}
deleteRecord :
    { record : Record
    , expect : Result Error () -> msg
    }
    -> Client
    -> AppCmd.Cmd msg
deleteRecord { record, expect } client =
    let
        mapper =
            mapResult expect (Decode.succeed ())
    in
    case Record.location record of
        Just path ->
            request
                { client = client
                , method = "DELETE"
                , headers = []
                , path = path
                , body = Http.emptyBody
                , timeout = Nothing
                , resolver = resolveWhatever
                , expect = mapper
                }

        Nothing ->
            Internal.Fetch mapper missingPrimaryKey


{-| Perform a request to a PostgREST instance resource.

The path can identify a plural resource such as `/posts` in which case an
[upsert](https://postgrest.org/en/stable/api.html?highlight=upsert#upsert)
operation will be performed, or a singular resource such as '/posts?id=eq.1'.

-}
request :
    { client : Client
    , method : String
    , headers : List Http.Header
    , path : String
    , body : Http.Body
    , resolver : Http.Resolver Error Value
    , expect : Result Error Value -> msg
    , timeout : Maybe Float
    }
    -> AppCmd.Cmd msg
request { client, method, headers, path, body, resolver, expect, timeout } =
    Client.task
        { client = client
        , method = method
        , headers = headers
        , path = path
        , body = body
        , resolver = resolver
        , timeout = timeout
        }
        |> Internal.Fetch expect



-- UTILS


missingPrimaryKey : Task Error a
missingPrimaryKey =
    Task.fail (Internal.RequestError "I cound't figure the primary key")


selects : Table -> List Selectable
selects table =
    Dict.values table.columns
        |> List.filterMap associationJoin
        |> (++) (Dict.keys table.columns |> List.map PG.attribute)


associationJoin : Column -> Maybe Selectable
associationJoin { constraint } =
    case constraint of
        ForeignKey foreignKey ->
            foreignKey.labelColumnName
                |> Maybe.map
                    (\n ->
                        PG.resource foreignKey.tableName
                            (PG.attributes [ n, "id" ])
                    )

        _ ->
            Nothing


tablePrimaryKeyName : Table -> Maybe String
tablePrimaryKeyName table =
    Dict.find (\_ column -> Field.isPrimaryKey column) table.columns
        |> Maybe.map Tuple.first


mapResult : (Result Error a -> msg) -> Decoder a -> Result Error Value -> msg
mapResult expect decoder result =
    result
        |> Result.andThen
            (Decode.decodeValue decoder >> Result.mapError DecodeError)
        |> expect


resolveWhatever : Http.Resolver Error Value
resolveWhatever =
    Http.bytesResolver
        (handleResponse (always (Ok Encode.null)))
