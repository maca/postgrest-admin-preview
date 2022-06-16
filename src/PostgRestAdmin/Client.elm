module PostgRestAdmin.Client exposing
    ( Client
    , toHostUrl
    , Table
    , getTable
    , tableName
    , fetchRecord
    , fetchRecordList
    , saveRecord
    , deleteRecord
    , request
    , requestMany
    , Collection
    , Error
    , errorToString
    , oneResolver
    , manyResolver
    , noneResolver
    , attempt
    , isAuthenticated
    , toJwtString
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
@docs request
@docs requestMany
@docs Collection

@docs Error
@docs errorToString

@docs oneResolver

@docs manyResolver
@docs noneResolver

@docs attempt


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
import Internal.Http as Internal
    exposing
        ( Error(..)
        , Response(..)
        , handleResponse
        )
import Internal.Schema as Schema exposing (Column, Constraint(..), Table)
import Json.Decode as Decode exposing (Decoder)
import Json.Encode as Encode
import Parser exposing ((|.), (|=), Parser)
import PostgRestAdmin.Cmd as AppCmd
import PostgRestAdmin.Record as Record exposing (Record)
import Postgrest.Client as PG exposing (Selectable)
import Task exposing (Task)
import Url exposing (Url)


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


{-| -}
type alias Collection a =
    { from : Int
    , to : Int
    , total : Int
    , list : List a
    }


type alias Count =
    { from : Int
    , to : Int
    , total : Int
    }


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

    import PostgRestAdmin.Cmd as AppCmd

    fetchOne : (Result Error Record -> msg) -> String -> Client -> AppCmd.Cmd Msg
    fetchOne tagger tableName client =
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
            decodeOne (Record.decoder table) >> expect
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
            attempt mapper <|
                task
                    { client = client
                    , method = "GET"
                    , headers =
                        [ header "Accept" "application/vnd.pgrst.object+json" ]
                    , path = "/" ++ tableName table ++ "?" ++ queryString
                    , body = Http.emptyBody
                    , resolver = oneResolver
                    , timeout = Nothing
                    }

        Nothing ->
            attempt mapper missingPrimaryKey


{-| Fetches a list of records for a given table.
`expect` param requires a function that returns a `Msg`.

    import PostgRestAdmin.Cmd as AppCmd

    fetchList : (Result Error (List Record) -> Msg) -> String -> Client -> AppCmd.Cmd Msg
    fetchList tagger tableName client =
        case getTable tableName client of
            Just table ->
                fetchRecordList
                    { client = client
                    , table = table
                    , params = []
                    , expect = tagger
                    }

            Nothing ->
                AppCmd.none

-}
fetchRecordList :
    { client : Client
    , table : Table
    , queryString : String
    , expect : Result Error (Collection Record) -> msg
    }
    -> AppCmd.Cmd msg
fetchRecordList { client, table, queryString, expect } =
    let
        selectString =
            PG.toQueryString [ PG.select (selects table) ]
    in
    attempt (decodeMany (Record.decoder table) >> expect) <|
        task
            { client = client
            , method = "GET"
            , headers = [ header "Prefer" "count=planned" ]
            , path = "/" ++ tableName table ++ "?" ++ selectString ++ "&" ++ queryString
            , body = Http.emptyBody
            , resolver = manyResolver
            , timeout = Nothing
            }


{-| Saves a record.
`expect` param requires a function that returns a `Msg`.

You can use [expectRecord](#expectRecord) to interpret the result as a
[Record](PostgRestAdmin.Record).

    import PostgRestAdmin.Cmd as AppCmd

    save : (Result Error () -> Msg) -> Record -> Maybe String -> Client -> AppCmd.Cmd Msg
    save tagger record id client =
        saveRecord
            { client = client
            , record = record
            , id = id
            , expect = tagger
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
        mapper =
            Result.map (always ()) >> expect

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
            , resolver = noneResolver
            , timeout = Nothing
            }
    in
    case id of
        Just _ ->
            attempt mapper (task params)

        Nothing ->
            attempt mapper (task { params | method = "POST" })


{-| Deletes a record.
`expect` param requires a function that returns a `Msg`.

    import PostgRestAdmin.Cmd as AppCmd

    delete : (Result Error Record -> Msg) -> Record -> Client -> AppCmd.Cmd Msg
    delete tagger record client =
        deleteRecord
            { client = client
            , record = record
            , expect = tagger
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
            Result.map (always ()) >> expect
    in
    case Record.location record of
        Just path ->
            attempt mapper <|
                task
                    { client = client
                    , method = "DELETE"
                    , headers = []
                    , path = path
                    , body = Http.emptyBody
                    , resolver = noneResolver
                    , timeout = Nothing
                    }

        Nothing ->
            attempt mapper missingPrimaryKey


{-| Perform a request
-}
request :
    { client : Client
    , method : String
    , headers : List Http.Header
    , path : String
    , body : Http.Body
    , decoder : Decoder a
    , expect : Result Error a -> msg
    }
    -> AppCmd.Cmd msg
request { client, method, headers, path, body, decoder, expect } =
    attempt (decodeOne decoder >> expect) <|
        Client.task
            { client = client
            , method = method
            , headers = headers
            , path = path
            , body = body
            , resolver = oneResolver
            , timeout = Nothing
            }


{-| -}
requestMany :
    { client : Client
    , method : String
    , headers : List Http.Header
    , path : String
    , body : Http.Body
    , decoder : Decoder a
    , expect : Result Error (Collection a) -> msg
    }
    -> AppCmd.Cmd msg
requestMany { client, method, headers, path, decoder, body, expect } =
    attempt (decodeMany decoder >> expect) <|
        Client.task
            { client = client
            , method = method
            , headers = headers
            , path = path
            , body = body
            , resolver = manyResolver
            , timeout = Nothing
            }


{-| Task to perform a request to a PostgREST instance resource.
-}
task :
    { client : Client
    , method : String
    , headers : List Http.Header
    , path : String
    , body : Http.Body
    , resolver : Http.Resolver Error body
    , timeout : Maybe Float
    }
    -> Task Error body
task { client, method, headers, resolver, path, body, timeout } =
    Client.task
        { client = client
        , method = method
        , headers = headers
        , path = path
        , body = body
        , resolver = resolver
        , timeout = timeout
        }


{-| -}
attempt :
    (Result Error Response -> msg)
    -> Task Error Response
    -> Internal.Cmd msg
attempt =
    Internal.Fetch



-- RESOLVE


{-| -}
oneResolver : Http.Resolver Error Response
oneResolver =
    Http.stringResolver <|
        handleResponse
            (\_ body ->
                if String.isEmpty body then
                    Ok (One Encode.null)

                else
                    case Decode.decodeString Decode.value body of
                        Err err ->
                            Err (DecodeError err)

                        Ok value ->
                            Ok (One value)
            )


{-| -}
manyResolver : Http.Resolver Error Response
manyResolver =
    Http.stringResolver <|
        handleResponse
            (\headers body ->
                let
                    count =
                        Dict.get "content-range" headers
                            |> Maybe.andThen
                                (Parser.run countParser >> Result.toMaybe)
                            |> Maybe.withDefault (Count 0 0 1)
                in
                if String.isEmpty body then
                    Ok (Many count [])

                else
                    case Decode.decodeString (Decode.list Decode.value) body of
                        Err err ->
                            Err (DecodeError err)

                        Ok values ->
                            Ok (Many count values)
            )


{-| -}
noneResolver : Http.Resolver Error Response
noneResolver =
    Http.stringResolver (handleResponse (\_ _ -> Ok None))



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



-- DECODE


decodeOne : Decoder a -> Result Error Response -> Result Error a
decodeOne decoder result =
    result
        |> Result.andThen
            (\response ->
                case response of
                    One value ->
                        Decode.decodeValue decoder value
                            |> Result.mapError DecodeError

                    Many _ _ ->
                        Err ExpectedRecord

                    None ->
                        Err ExpectedRecord
            )


decodeMany :
    Decoder a
    -> Result Error Response
    -> Result Error (Collection a)
decodeMany decoder result =
    result
        |> Result.andThen
            (\response ->
                case response of
                    Many { from, to, total } list ->
                        Encode.list identity list
                            |> Decode.decodeValue (Decode.list decoder)
                            |> Result.mapError DecodeError
                            |> Result.map (Collection from to total)

                    One _ ->
                        Err ExpectedRecordList

                    None ->
                        Err ExpectedRecordList
            )


countParser : Parser Count
countParser =
    Parser.succeed (\( from, to ) count -> Count from to (Maybe.withDefault to count))
        |= Parser.oneOf
            [ Parser.succeed Tuple.pair
                |= Parser.int
                |. Parser.symbol "-"
                |= Parser.int
            , Parser.succeed ( 0, 0 )
                |. Parser.symbol "*"
            ]
        |. Parser.symbol "/"
        |= Parser.oneOf
            [ Parser.map Just Parser.int
            , Parser.map (always Nothing) (Parser.symbol "*")
            ]
