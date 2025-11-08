module PostgRestAdmin.Client exposing
    ( Client
    , Table, getTable, tableName
    , fetchRecord, fetchRecordList, saveRecord, deleteRecord, request, requestMany, Collection
    , Error, errorToString
    , oneResolver, manyResolver, noneResolver
    , task, decodeOne, decodeMany
    , isAuthenticated, toJwtString, authHeader
    , AuthError(..), AuthScheme(..), Response(..), authFailed, authSchemeConfig, authUrl, authUrlDecoder, basic, encoder, endpoint, fetchRecord2, fetchSchema, handleJsonResponse, handleResponse, init, jwt, jwtDecoder, listableColumns, listingSelects, logout, removeLeadingOrTrailingSlash, schemaIsLoaded, toError, unset, updateJwt
    )

{-|


# Client

@docs Client


# Table

@docs Table, getTable, tableName


# Requests

Note that the request functions **do not produce a vanilla Elm
[Cmd](https://package.elm-lang.org/packages/elm/core/latest/Platform-Cmd#Cmd)**
but a [PostgRestAdmin.Cmd](PostgRestAdmin-Cmd).

@docs fetchRecord, fetchRecordList, saveRecord, deleteRecord, request, requestMany, Collection


# Errors

@docs Error, errorToString


# Resolvers

@docs oneResolver, manyResolver, noneResolver


# Tasks

@docs task, decodeOne, decodeMany


# Authentication

@docs isAuthenticated, toJwtString, authHeader

-}

import Dict exposing (Dict)
import Dict.Extra as Dict
import Http
import Internal.Cmd as AppCmd
import Internal.Field as Field
import Internal.Http
import Internal.Schema as Schema exposing (Column, Constraint(..), Table)
import Internal.Value exposing (Value(..))
import Json.Decode as Decode exposing (Decoder)
import Json.Encode as Encode
import Parser exposing ((|.), (|=), Parser)
import PostgRestAdmin.Record as Record exposing (Record)
import Postgrest.Client as PG exposing (Selectable)
import String.Extra as String
import Task exposing (Task)
import Url exposing (Url)



-- TYPES


type alias Error =
    Internal.Http.Error


type Response
    = One Decode.Value
    | Many Count (List Decode.Value)
    | None


type AuthScheme
    = Jwt PG.JWT
    | Unset


type AuthError
    = Forbidden
    | Unauthorized
    | ServerError Int
    | NetworkError


{-| Represents a client for a PostgREST instance, including authentication
params.

See [Config](PostgRestAdmin-Config) and
[Config.FormAuth](PostgRestAdmin-Config-FormAuth) for authentication
configuration options.

-}
type alias Client =
    { host : Url
    , authScheme : AuthScheme
    , schema : Schema.Schema
    }


{-| Represents a PostgREST table.
-}
type alias Table =
    Schema.Table


type alias Count =
    { from : Int
    , to : Int
    , total : Int
    }


{-| -}
type alias Collection a =
    { from : Int
    , to : Int
    , total : Int
    , records : List a
    }


{-| Does the client has a valid JWT?
-}
isAuthenticated : Client -> Bool
isAuthenticated { authScheme } =
    authSchemeIsAuthenticated authScheme


authSchemeIsAuthenticated : AuthScheme -> Bool
authSchemeIsAuthenticated authScheme =
    toJwt authScheme |> Maybe.map (always True) |> Maybe.withDefault False


{-| Obtain the JWT as a string.
-}
toJwtString : Client -> Maybe String
toJwtString { authScheme } =
    toJwt authScheme
        |> Maybe.map PG.jwtString


toJwt : AuthScheme -> Maybe PG.JWT
toJwt authScheme =
    case authScheme of
        Jwt token ->
            Just token

        Unset ->
            Nothing


{-| Generate an authorization header.
-}
authHeader : Client -> Maybe Http.Header
authHeader client =
    toJwtString client
        |> Maybe.map (\jwtStr -> Http.header "Authorization" ("Bearer " ++ jwtStr))


{-| Obtain a table from the table name.
-}
getTable : String -> Client -> Maybe Table
getTable name { schema } =
    Dict.get name schema


{-| Obtain the name of a table
-}
tableName : Table -> String
tableName table =
    table.name



-- AUTH CONFIG


authSchemeConfig : Decoder AuthScheme
authSchemeConfig =
    Decode.oneOf
        [ Decode.field "jwt" Decode.string
            |> Decode.map (\token -> Jwt (PG.jwt token))
        , Decode.succeed Unset
        ]


basic : AuthScheme -> AuthScheme
basic auth =
    auth


jwt : String -> AuthScheme
jwt tokenStr =
    Jwt (PG.jwt tokenStr)


unset : AuthScheme
unset =
    Unset


authUrl : String -> Decoder AuthScheme -> Decoder AuthScheme
authUrl urlStr =
    Decode.andThen (authUrlDecoder urlStr)


authUrlDecoder : String -> AuthScheme -> Decoder AuthScheme
authUrlDecoder _ authScheme =
    Decode.succeed authScheme


encoder : (Dict String String -> Encode.Value) -> Decoder AuthScheme -> Decoder AuthScheme
encoder _ =
    Decode.map identity


jwtDecoder : Decoder String -> Decoder AuthScheme -> Decoder AuthScheme
jwtDecoder _ =
    Decode.map identity



-- CLIENT FUNCTIONS


init : Url -> AuthScheme -> Client
init url authScheme =
    { host = url
    , authScheme = authScheme
    , schema = Dict.empty
    }


updateJwt : String -> Client -> Client
updateJwt tokenStr client =
    { client | authScheme = updateAuthSchemeJwt tokenStr client.authScheme }


updateAuthSchemeJwt : String -> AuthScheme -> AuthScheme
updateAuthSchemeJwt tokenStr _ =
    Jwt (PG.jwt tokenStr)


logout : Client -> Client
logout client =
    { client
        | authScheme = clearAuthSchemeJwt client.authScheme
        , schema = Dict.empty
    }


clearAuthSchemeJwt : AuthScheme -> AuthScheme
clearAuthSchemeJwt _ =
    Unset


authFailed : Client -> Client
authFailed client =
    { client | authScheme = failAuthScheme client.authScheme }


failAuthScheme : AuthScheme -> AuthScheme
failAuthScheme authScheme =
    case authScheme of
        Jwt _ ->
            Unset

        Unset ->
            Unset


schemaIsLoaded : Client -> Bool
schemaIsLoaded { schema } =
    not (Dict.isEmpty schema)


fetchSchema :
    { a | tables : List String, tableAliases : Dict String String }
    -> Client
    -> Task Error Schema.Schema
fetchSchema config client =
    task
        { client = client
        , method = "GET"
        , headers = []
        , path = "/"
        , body = Http.emptyBody
        , resolver =
            Http.stringResolver
                (handleJsonResponse (Schema.decoder config))
        , timeout = Nothing
        }


{-| Transform [Error](#Error) to an error explanation.
-}
errorToString : Error -> String
errorToString error =
    case error of
        Internal.Http.HttpError httpError ->
            case httpError of
                Http.BadUrl msg ->
                    "Invalid URL:" ++ msg

                Http.Timeout ->
                    "The requested timed out. Please try again."

                Http.NetworkError ->
                    "Network Error: do you have an internet connection?"

                Http.BadStatus status ->
                    "Bad status: " ++ String.fromInt status

                Http.BadBody msg ->
                    "Response error: " ++ msg

        Internal.Http.DecodeError err ->
            Decode.errorToString err

        Internal.Http.RequestError msg ->
            msg

        Internal.Http.ExpectedRecord ->
            "Expected a record"

        Internal.Http.ExpectedRecordList ->
            "Expected a list of records"

        Internal.Http.AuthError ->
            "There was an error authorising your credentials."


handleJsonResponse : Decoder a -> Http.Response String -> Result Error a
handleJsonResponse decoder =
    handleResponse
        (\_ body ->
            case Decode.decodeString decoder body of
                Err err ->
                    Err (Internal.Http.DecodeError err)

                Ok result ->
                    Ok result
        )


handleResponse :
    (Dict String String -> body -> Result Error a)
    -> Http.Response body
    -> Result Error a
handleResponse toResult response =
    case response of
        Http.BadUrl_ url ->
            Err (Internal.Http.HttpError (Http.BadUrl url))

        Http.Timeout_ ->
            Err (Internal.Http.HttpError Http.Timeout)

        Http.BadStatus_ { statusCode } _ ->
            case statusCode of
                401 ->
                    Err Internal.Http.AuthError

                _ ->
                    Err (Internal.Http.HttpError (Http.BadStatus statusCode))

        Http.NetworkError_ ->
            Err (Internal.Http.HttpError Http.NetworkError)

        Http.GoodStatus_ { headers } body ->
            toResult headers body


toError : Result x a -> Maybe x
toError =
    Internal.Http.toError


removeLeadingOrTrailingSlash : String -> String
removeLeadingOrTrailingSlash =
    Internal.Http.removeLeadingOrTrailingSlash



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
            AppCmd.wrap <|
                Task.attempt mapper <|
                    task
                        { client = client
                        , method = "GET"
                        , headers =
                            [ Http.header "Accept" "application/vnd.pgrst.object+json" ]
                        , path = "/" ++ tableName table ++ "?" ++ queryString
                        , body = Http.emptyBody
                        , resolver = oneResolver
                        , timeout = Nothing
                        }

        Nothing ->
            AppCmd.wrap (Task.attempt mapper missingPrimaryKey)


fetchRecord2 :
    { client : Client
    , table : Table
    , expect : Result Http.Error Decode.Value -> msg
    , id : String
    }
    -> AppCmd.Cmd msg
fetchRecord2 { client, table, expect, id } =
    case tablePrimaryKeyName table of
        Just primaryKeyName ->
            let
                queryStr =
                    PG.toQueryString
                        [ PG.select (selects table)
                        , PG.eq (PG.string id) |> PG.param primaryKeyName
                        , PG.limit 1
                        ]
            in
            AppCmd.wrap <|
                Http.request
                    { method = "GET"
                    , headers =
                        List.filterMap identity
                            [ authHeader client
                            , Just (Http.header "Accept" "application/vnd.pgrst.object+json")
                            ]
                    , url = endpoint client ("/" ++ tableName table ++ "?" ++ queryStr)
                    , body = Http.emptyBody
                    , expect = Http.expectJson expect (Decode.index 0 Decode.value)
                    , timeout = Nothing
                    , tracker = Nothing
                    }

        Nothing ->
            Debug.todo "crash"



-- Internal.Fetch expect missingPrimaryKey


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
    AppCmd.wrap <|
        Task.attempt (decodeMany (Record.decoder table) >> expect) <|
            task
                { client = client
                , method = "GET"
                , headers = [ Http.header "Prefer" "count=planned" ]
                , path = "/" ++ tableName table ++ "?" ++ selectString ++ "&" ++ queryString
                , body = Http.emptyBody
                , resolver = manyResolver
                , timeout = Nothing
                }


{-| Saves a record.
`expect` param requires a function that returns a `Msg`.

You can use [expectRecord](#expectRecord) to interpret the result as a
[Record](PostgRestAdmin-Record).

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
    , expect : Result Error Record -> msg
    }
    -> AppCmd.Cmd msg
saveRecord { client, record, id, expect } =
    let
        table =
            Record.getTable record

        mapper =
            decodeOne (Record.decoder table) >> expect

        queryString =
            PG.toQueryString [ PG.select (selects record.table) ]

        params =
            { client = client
            , method = "PATCH"
            , headers =
                [ Http.header "Prefer" "return=representation"
                , Http.header "Accept" "application/vnd.pgrst.object+json"
                ]
            , path = "/" ++ Record.tableName record
            , body = Http.jsonBody (Record.encode record)
            , resolver = oneResolver
            , timeout = Nothing
            }
    in
    case id of
        Just recordId ->
            AppCmd.wrap <|
                Task.attempt mapper <|
                    task
                        { params
                            | path =
                                params.path
                                    ++ "?id=eq."
                                    ++ recordId
                                    ++ "&"
                                    ++ queryString
                        }

        Nothing ->
            AppCmd.wrap <|
                Task.attempt mapper <|
                    task
                        { params
                            | method = "POST"
                            , path = params.path ++ "?" ++ queryString
                        }


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
            AppCmd.wrap <|
                Task.attempt mapper <|
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
            AppCmd.wrap (Task.attempt mapper missingPrimaryKey)


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
    AppCmd.wrap <|
        Task.attempt (decodeOne decoder >> expect) <|
            task
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
    AppCmd.wrap <|
        Task.attempt (decodeMany decoder >> expect) <|
            task
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
task { client, method, headers, path, body, resolver, timeout } =
    case authHeader client of
        Just auth ->
            Http.task
                { method = method
                , headers = auth :: headers
                , url = endpoint client path
                , body = body
                , resolver = resolver
                , timeout = timeout
                }
                |> Task.onError fail

        Nothing ->
            fail Internal.Http.AuthError


endpoint : Client -> String -> String
endpoint { host } path =
    Url.toString
        { host
            | path =
                "/"
                    ++ ([ host.path, path ]
                            |> List.filterMap
                                (removeLeadingOrTrailingSlash >> String.nonBlank)
                            |> String.join "/"
                       )
        }


fail : Error -> Task Error a
fail err =
    case err of
        Internal.Http.AuthError ->
            Task.fail Internal.Http.AuthError

        _ ->
            Task.fail err



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
                            Err (Internal.Http.DecodeError err)

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
                            Err (Internal.Http.DecodeError err)

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
    Task.fail (Internal.Http.RequestError "I cound't figure the primary key")


selects : Table -> List Selectable
selects table =
    Dict.values table.columns
        |> List.filterMap associationJoin
        |> (++) (Dict.keys table.columns |> List.map PG.attribute)


listableColumns : Table -> Dict String Column
listableColumns table =
    table.columns
        |> Dict.filter
            (\_ column ->
                case column.value of
                    PText _ ->
                        False

                    PJson _ ->
                        False

                    Unknown _ ->
                        False

                    _ ->
                        True
            )


listingSelects : Table -> List Selectable
listingSelects table =
    Dict.values table.columns
        |> List.filterMap associationJoin
        |> (++)
            (listableColumns table
                |> Dict.keys
                |> List.map PG.attribute
            )


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


{-| -}
decodeOne : Decoder a -> Result Error Response -> Result Error a
decodeOne decoder result =
    result
        |> Result.andThen
            (\response ->
                case response of
                    One value ->
                        let
                            mapper =
                                Decode.decodeValue decoder >> Result.mapError Internal.Http.DecodeError
                        in
                        mapper value

                    Many _ _ ->
                        Err Internal.Http.ExpectedRecord

                    None ->
                        Err Internal.Http.ExpectedRecord
            )


{-| -}
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
                        let
                            mapper =
                                Decode.decodeValue (Decode.list decoder)
                                    >> Result.mapError Internal.Http.DecodeError
                        in
                        Encode.list identity list
                            |> mapper
                            |> Result.map (Collection from to total)

                    One _ ->
                        Err Internal.Http.ExpectedRecordList

                    None ->
                        Err Internal.Http.ExpectedRecordList
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
