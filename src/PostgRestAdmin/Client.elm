module PostgRestAdmin.Client exposing
    ( Client
    , Table, getTable, tableName
    , fetchRecord, fetchRecordList, saveRecord, deleteRecord, request, requestMany, Collection
    , Error(..), errorToString
    , oneResolver, manyResolver, noneResolver
    , task, decodeOne, decodeMany
    , isAuthenticated, toJwtString, authHeader
    , AuthScheme(..), Response(..), authFailed, authSchemeConfig, authUrl, authUrlDecoder, basic, encoder, endpoint, fetchSchema, init, jwt, jwtDecoder, listableColumns, listingSelects, logout, removeLeadingOrTrailingSlash, schemaIsLoaded, toError, unset, updateJwt
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

@docs saveRecord2

-}

import Dict exposing (Dict)
import Dict.Extra as Dict
import Http
import Internal.Cmd as AppCmd
import Internal.Field as Field
import Internal.Schema as Schema exposing (Column, Constraint(..), Table)
import Internal.Value exposing (Value(..))
import Json.Decode as Decode exposing (Decoder)
import Json.Encode as Encode
import Parser exposing ((|.), (|=), Parser)
import PostgRestAdmin.Record as Record exposing (Record)
import Postgrest.Client as PG exposing (Selectable)
import Regex
import String.Extra as String
import Task exposing (Task)
import Url exposing (Url)



-- TYPES


type Error
    = BadUrl String
    | Timeout
    | NetworkError
    | BadStatus Int
    | DecodeError Decode.Error
    | RequestError String
    | ExpectedRecord
    | ExpectedRecordList
    | Forbidden
    | Unauthorized


type Response
    = One Decode.Value
    | Many Count (List Decode.Value)
    | None


type AuthScheme
    = Jwt PG.JWT
    | Unset


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
    Http.task
        { method = "GET"
        , headers = List.filterMap identity [ authHeader client ]
        , url = endpoint client "/"
        , body = Http.emptyBody
        , resolver = jsonResolver (Schema.decoder config)
        , timeout = Nothing
        }


{-| Transform [Error](#Error) to an error explanation.
-}
errorToString : Error -> String
errorToString error =
    case error of
        BadUrl msg ->
            "Invalid URL:" ++ msg

        Timeout ->
            "The requested timed out. Please try again."

        NetworkError ->
            "Network Error: do you have an internet connection?"

        BadStatus status ->
            "Bad status: " ++ String.fromInt status

        DecodeError err ->
            Decode.errorToString err

        RequestError msg ->
            msg

        ExpectedRecord ->
            "Expected a record"

        ExpectedRecordList ->
            "Expected a list of records"

        Forbidden ->
            "Forbidden: you don't have permission to access this resource."

        Unauthorized ->
            "Unauthorized: please check your credentials."


toError : Result x a -> Maybe x
toError result =
    case result of
        Err err ->
            Just err

        _ ->
            Nothing


removeLeadingOrTrailingSlash : String -> String
removeLeadingOrTrailingSlash =
    Regex.replace
        (Maybe.withDefault Regex.never (Regex.fromString "^/|/$"))
        (always "")



-- VIEW


jsonResolver : Decode.Decoder a -> Http.Resolver Error a
jsonResolver decoder =
    Http.stringResolver
        (\response ->
            case response of
                Http.BadUrl_ url ->
                    Err (BadUrl url)

                Http.Timeout_ ->
                    Err Timeout

                Http.NetworkError_ ->
                    Err NetworkError

                Http.BadStatus_ { statusCode } _ ->
                    case statusCode of
                        401 ->
                            Err Unauthorized

                        403 ->
                            Err Forbidden

                        _ ->
                            Err (BadStatus statusCode)

                Http.GoodStatus_ _ body ->
                    Decode.decodeString decoder body
                        |> Result.mapError DecodeError
        )


{-| Fetches a record for a given table.
Requires a decoder for the response and an `expect` function that returns a `Msg`.

    import PostgRestAdmin.Cmd as AppCmd
    import Json.Decode as Decode

    fetchOne : (Result Error Decode.Value -> msg) -> String -> String -> Client -> AppCmd.Cmd Msg
    fetchOne tagger tableName recordId client =
        case getTable tableName client of
            Just table ->
                fetchRecord
                    { client = client
                    , table = table
                    , id = recordId
                    , decoder = Decode.value
                    , expect = tagger
                    }

            Nothing ->
                AppCmd.none

-}
fetchRecord :
    { client : Client
    , table : Table
    , id : String
    , decoder : Decode.Decoder a
    , expect : Result Error a -> msg
    }
    -> AppCmd.Cmd msg
fetchRecord { client, table, id, expect, decoder } =
    let
        queryStr =
            PG.toQueryString
                (List.filterMap identity
                    [ Just (PG.select (selects table))
                    , tablePrimaryKeyName table
                        |> Maybe.map
                            (\key -> PG.eq (PG.string id) |> PG.param key)
                    , Just (PG.limit 1)
                    ]
                )
    in
    AppCmd.wrap <|
        Task.attempt expect <|
            Http.task
                { method = "GET"
                , headers =
                    List.filterMap identity
                        [ authHeader client
                        , Just (Http.header "Accept" "application/vnd.pgrst.object+json")
                        ]
                , url = endpoint client ("/" ++ tableName table ++ "?" ++ queryStr)
                , body = Http.emptyBody
                , resolver = jsonResolver decoder
                , timeout = Nothing
                }



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


saveRecord :
    { client : Client
    , body : Encode.Value
    , table : Table
    , id : Maybe String
    , decoder : Decode.Decoder a
    , expect : Result Error a -> msg
    }
    -> AppCmd.Cmd msg
saveRecord { client, body, table, decoder, id, expect } =
    let
        queryString =
            PG.toQueryString [ PG.select (selects table) ]

        ( path, method ) =
            case id of
                Just recordId ->
                    ( "/" ++ table.name ++ "?id=eq." ++ recordId ++ "&" ++ queryString
                    , "PATCH"
                    )

                Nothing ->
                    ( "/" ++ table.name ++ "?" ++ queryString
                    , "POST"
                    )
    in
    AppCmd.wrap <|
        Task.attempt expect <|
            Http.task
                { method = method
                , headers =
                    List.filterMap identity
                        [ authHeader client
                        , Just (Http.header "Prefer" "return=representation")
                        , Just (Http.header "Accept" "application/vnd.pgrst.object+json")
                        ]
                , url = endpoint client path
                , body = Http.jsonBody body
                , resolver = jsonResolver decoder
                , timeout = Nothing
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
            fail Unauthorized


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
    Task.fail err



-- RESOLVE


{-| -}
oneResolver : Http.Resolver Error Response
oneResolver =
    Http.stringResolver <|
        \response ->
            case response of
                Http.BadUrl_ url ->
                    Err (BadUrl url)

                Http.Timeout_ ->
                    Err Timeout

                Http.BadStatus_ { statusCode } _ ->
                    case statusCode of
                        401 ->
                            Err Unauthorized

                        403 ->
                            Err Forbidden

                        _ ->
                            Err (BadStatus statusCode)

                Http.NetworkError_ ->
                    Err NetworkError

                Http.GoodStatus_ _ body ->
                    if String.isEmpty body then
                        Ok (One Encode.null)

                    else
                        case Decode.decodeString Decode.value body of
                            Err err ->
                                Err (DecodeError err)

                            Ok value ->
                                Ok (One value)


{-| -}
manyResolver : Http.Resolver Error Response
manyResolver =
    Http.stringResolver <|
        \response ->
            case response of
                Http.BadUrl_ url ->
                    Err (BadUrl url)

                Http.Timeout_ ->
                    Err Timeout

                Http.BadStatus_ { statusCode } _ ->
                    case statusCode of
                        401 ->
                            Err Unauthorized

                        403 ->
                            Err Forbidden

                        _ ->
                            Err (BadStatus statusCode)

                Http.NetworkError_ ->
                    Err NetworkError

                Http.GoodStatus_ { headers } body ->
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


{-| -}
noneResolver : Http.Resolver Error Response
noneResolver =
    Http.stringResolver <|
        \response ->
            case response of
                Http.BadUrl_ url ->
                    Err (BadUrl url)

                Http.Timeout_ ->
                    Err Timeout

                Http.BadStatus_ { statusCode } _ ->
                    case statusCode of
                        401 ->
                            Err Unauthorized

                        403 ->
                            Err Forbidden

                        _ ->
                            Err (BadStatus statusCode)

                Http.NetworkError_ ->
                    Err NetworkError

                Http.GoodStatus_ _ _ ->
                    Ok None



-- UTILS


missingPrimaryKey : Task Error a
missingPrimaryKey =
    Task.fail (RequestError "I cound't figure the primary key")


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
                                Decode.decodeValue decoder >> Result.mapError DecodeError
                        in
                        mapper value

                    Many _ _ ->
                        Err ExpectedRecord

                    None ->
                        Err ExpectedRecord
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
                                    >> Result.mapError DecodeError
                        in
                        Encode.list identity list
                            |> mapper
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
