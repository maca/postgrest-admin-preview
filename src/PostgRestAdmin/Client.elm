module PostgRestAdmin.Client exposing
    ( Client, init
    , Count
    , fetchRecord, fetchRecords, saveRecord, deleteRecord
    , request, requestMany, task
    , count
    , Error(..), errorToString
    , AuthScheme(..), jwt, unset, toJwtString, authHeader, updateJwt, logout
    , fetchSchema, schemaIsLoaded
    , endpoint
    , authSchemeConfig, authUrl, authUrlDecoder, basic, encoder, jwtDecoder
    , associationJoin, bytesRequest, chunk, fetchParentLabel, jsonResolver, recordDecoder
    )

{-| PostgREST client for Elm applications.


# Client

@docs Client, init


# Requests

@docs Count
@docs fetchRecord, fetchRecords, saveRecord, deleteRecord
@docs request, requestMany, task
@docs count


# Errors

@docs Error, errorToString


# Authentication

@docs AuthScheme, jwt, unset, toJwtString, authHeader, updateJwt, logout


# Schema

@docs fetchSchema, schemaIsLoaded


# Resolvers

@docs Response


# Tasks


# Utilities

@docs endpoint


# Advanced

Functions for advanced configuration and internal use.

@docs authSchemeConfig, authUrl, authUrlDecoder, basic, encoder, jwtDecoder, removeLeadingOrTrailingSlash

-}

import Bytes exposing (Bytes)
import Dict exposing (Dict)
import Dict.Extra as Dict
import Http
import Internal.Cmd as AppCmd
import Internal.Schema as Schema exposing (Column, Constraint(..), Table, Value(..), buildReferences)
import Json.Decode as Decode exposing (Decoder)
import Json.Encode as Encode
import Parser exposing ((|.), (|=), Parser)
import Postgrest.Client as PG exposing (Selectable)
import Regex
import String.Extra as String
import Task exposing (Task)
import Url exposing (Url)
import Url.Builder



-- TYPES


type Error
    = BadUrl String
    | Timeout
    | NetworkError
    | BadStatus Int
    | BadHeader String
    | DecodeError Decode.Error
    | RequestError String
    | Forbidden
    | Unauthorized


type alias Count =
    { from : Int
    , to : Int
    , total : Int
    }


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
    , headers : List Http.Header
    }


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
    identity


jwtDecoder : Decoder String -> Decoder AuthScheme -> Decoder AuthScheme
jwtDecoder _ =
    identity



-- CLIENT FUNCTIONS


init : Url -> AuthScheme -> List Http.Header -> Client
init url authScheme headers =
    { host = url
    , authScheme = authScheme
    , schema = Dict.empty
    , headers = headers
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
        , resolver = jsonResolver (Schema.decoder config)
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

        Forbidden ->
            "Forbidden: you don't have permission to access this resource."

        Unauthorized ->
            "Unauthorized: please check your credentials."

        BadHeader headerName ->
            "The header \"" ++ headerName ++ "\" could not be read."


removeLeadingOrTrailingSlash : String -> String
removeLeadingOrTrailingSlash =
    Regex.replace
        (Maybe.withDefault Regex.never (Regex.fromString "^/|/$"))
        (always "")



-- VIEW


jsonResolver : Decode.Decoder a -> Http.Resolver Error a
jsonResolver decoder =
    Http.stringResolver
        (resolve
            (\_ body ->
                Decode.decodeString decoder body
                    |> Result.mapError DecodeError
            )
        )


countResolver : Decode.Decoder a -> Http.Resolver Error ( a, Count )
countResolver decoder =
    Http.stringResolver
        (resolve
            (\{ headers } body ->
                case
                    Dict.get "content-range" headers
                        |> Maybe.map (Parser.run countParser)
                of
                    Just (Ok recordCount) ->
                        Decode.decodeString
                            (decoder |> Decode.map (\a -> ( a, recordCount )))
                            body
                            |> Result.mapError DecodeError

                    _ ->
                        Err (BadHeader "content-range")
            )
        )


resolve : (Http.Metadata -> body -> Result Error value) -> Http.Response body -> Result Error value
resolve fn response =
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

        Http.GoodStatus_ metadata body ->
            fn metadata body


{-| Fetches a record for a given table.
Requires a decoder for the response and an `expect` function that returns a `Msg`.

    import Dict
    import Json.Decode as Decode
    import PostgRestAdmin.Cmd as AppCmd

    fetchOne : (Result Error Decode.Value -> msg) -> String -> String -> Client -> AppCmd.Cmd Msg
    fetchOne tagger tableName recordId client =
        case Dict.get tableName client.schema of
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
fetchRecord : { client : Client, table : Table, id : String } -> Task Error Schema.Record
fetchRecord { client, table, id } =
    let
        queryStr =
            PG.toQueryString
                (List.filterMap identity
                    [ Just (PG.select (selects table))
                    , Schema.tablePrimaryKeyName table
                        |> Maybe.map
                            (\key -> PG.eq (PG.string id) |> PG.param key)
                    , Just (PG.limit 1)
                    ]
                )
    in
    task
        { client = client
        , method = "GET"
        , headers = [ Http.header "Accept" "application/vnd.pgrst.object+json" ]
        , path = "/" ++ table.name ++ "?" ++ queryStr
        , body = Http.emptyBody
        , resolver = jsonResolver (recordDecoder table)
        }


fetchRecords :
    { client : Client
    , table : Table
    , path : String
    }
    -> Task Error ( List Schema.Record, Count )
fetchRecords { client, table, path } =
    task
        { client = client
        , method = "GET"
        , headers = [ Http.header "Prefer" "count=exact" ]
        , path = path
        , body = Http.emptyBody
        , resolver = countResolver (Decode.list (recordDecoder table))
        }


fetchParentLabel :
    Client
    ->
        { parentLabelColumn : String
        , parentPrimaryKey : String
        , parentId : String
        , parentTable : { b | name : String }
        }
    -> Task.Task Error String
fetchParentLabel client params =
    let
        selectedCols =
            PG.select
                [ PG.attribute params.parentLabelColumn ]

        queryString =
            PG.toQueryString
                [ selectedCols
                , PG.param params.parentPrimaryKey (PG.eq (PG.string params.parentId))
                , PG.limit 1
                ]
    in
    task
        { client = client
        , method = "GET"
        , headers = [ Http.header "Accept" "application/vnd.pgrst.object+json" ]
        , path = "/" ++ params.parentTable.name ++ "?" ++ queryString
        , body = Http.emptyBody
        , resolver = jsonResolver (Decode.field params.parentLabelColumn Decode.string)
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
            case Maybe.map2 Tuple.pair (Schema.tablePrimaryKeyName table) id of
                Just ( pkName, recordId ) ->
                    ( "/" ++ table.name ++ "?" ++ pkName ++ "=eq." ++ recordId ++ "&" ++ queryString
                    , "PATCH"
                    )

                Nothing ->
                    ( "/" ++ table.name ++ "?" ++ queryString
                    , "POST"
                    )
    in
    AppCmd.wrap <|
        Task.attempt expect <|
            task
                { client = client
                , method = method
                , headers =
                    [ Http.header "Prefer" "return=representation"
                    , Http.header "Accept" "application/vnd.pgrst.object+json"
                    ]
                , path = path
                , body = Http.jsonBody body
                , resolver = jsonResolver decoder
                }


{-| Deletes a record.
`expect` param requires a function that returns a `Msg`.

    import Dict
    import PostgRestAdmin.Cmd as AppCmd

    delete : (Result Error () -> Msg) -> String -> String -> Client -> AppCmd.Cmd Msg
    delete tagger tableName recordId client =
        case Dict.get tableName client.schema of
            Just table ->
                deleteRecord
                    { client = client
                    , table = table
                    , id = recordId
                    , expect = tagger
                    }

            Nothing ->
                AppCmd.none

-}
deleteRecord :
    { client : Client
    , table : Table
    , id : String
    , expect : Result Error () -> msg
    }
    -> AppCmd.Cmd msg
deleteRecord { client, table, id, expect } =
    let
        queryStr =
            PG.toQueryString
                (List.filterMap identity
                    [ Schema.tablePrimaryKeyName table
                        |> Maybe.map
                            (\key -> PG.eq (PG.string id) |> PG.param key)
                    ]
                )
    in
    AppCmd.wrap <|
        Task.attempt expect <|
            task
                { client = client
                , method = "DELETE"
                , headers = []
                , path = "/" ++ table.name ++ "?" ++ queryStr
                , body = Http.emptyBody
                , resolver = jsonResolver (Decode.succeed ())
                }


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
request params =
    task
        { client = params.client
        , method = params.method
        , headers = params.headers
        , path = params.path
        , body = params.body
        , resolver = jsonResolver params.decoder
        }
        |> Task.attempt params.expect
        |> AppCmd.wrap


task :
    { a
        | client : Client
        , method : String
        , headers : List Http.Header
        , path : String
        , body : Http.Body
        , resolver : Http.Resolver Error b
    }
    -> Task Error b
task { client, method, headers, path, body, resolver } =
    Http.task
        { method = method
        , headers =
            List.concat
                [ client.headers
                , authHeader client
                    |> Maybe.map List.singleton
                    |> Maybe.withDefault []
                , headers
                ]
        , url = endpoint client path
        , body = body
        , resolver = resolver
        , timeout = Nothing
        }


chunk : Client -> Table -> Maybe String -> List String -> Task.Task Error (List Schema.Record)
chunk client table primaryKeyName ids =
    let
        existenceQuery =
            List.filterMap identity
                [ primaryKeyName
                    |> Maybe.map
                        (\pkn ->
                            let
                                conds =
                                    ids
                                        |> List.map (\id -> pkn ++ ".eq." ++ id)
                                        |> String.join ","
                            in
                            Url.Builder.string "or" <| "(" ++ conds ++ ")"
                        )
                ]
    in
    task
        { client = client
        , method = "GET"
        , headers = []
        , path =
            Url.Builder.absolute [ table.name ] existenceQuery
        , body = Http.emptyBody
        , resolver = jsonResolver (Decode.list (Decode.dict Schema.valueDecoder))
        }


bytesRequest :
    { a
        | client : Client
        , method : String
        , headers : List Http.Header
        , path : String
        , body : Http.Body
    }
    -> Task Error Bytes
bytesRequest { client, method, headers, path, body } =
    task
        { client = client
        , method = method
        , headers = headers
        , path = path
        , body = body
        , resolver = Http.bytesResolver (resolve (always Ok))
        }


{-| -}
requestMany :
    { client : Client
    , method : String
    , headers : List Http.Header
    , path : String
    , body : Http.Body
    , decoder : Decoder a
    , expect : Result Error ( List a, Count ) -> msg
    }
    -> AppCmd.Cmd msg
requestMany { client, method, headers, path, decoder, body, expect } =
    AppCmd.wrap <|
        Task.attempt expect <|
            task
                { client = client
                , method = method
                , headers = headers
                , path = path
                , body = body
                , resolver = countResolver (Decode.list decoder)
                }


count : { client : Client, path : String } -> Task Error Int
count { client, path } =
    task
        { client = client
        , method = "HEAD"
        , headers = [ Http.header "Prefer" "count=exact" ]
        , path = path
        , body = Http.emptyBody
        , resolver =
            Http.bytesResolver
                (resolve
                    (\{ headers } _ ->
                        case
                            Dict.get "content-range" headers
                                |> Maybe.map (Parser.run countParser)
                        of
                            Just (Ok recordCount) ->
                                Ok recordCount.total

                            _ ->
                                Err (BadHeader "content-range")
                    )
                )
        }


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



-- UTILS


selects : Table -> List Selectable
selects table =
    List.concat
        [ Dict.values table.columns
            |> List.filterMap associationJoin
        , Dict.keys table.columns
            |> List.map PG.attribute
        ]


associationJoin : Column -> Maybe Selectable
associationJoin { constraint } =
    case constraint of
        ForeignKey fk ->
            fk.labelColumnName
                |> Maybe.map
                    (\n ->
                        PG.resource fk.tableName
                            (PG.attributes [ n, fk.primaryKeyName ])
                    )

        _ ->
            Nothing



-- DECODE


recordDecoder : Table -> Decode.Decoder Schema.Record
recordDecoder table =
    table.columns
        |> Dict.foldl
            (\name col ->
                Decode.map2
                    (Maybe.map (Dict.insert name) >> Maybe.withDefault identity)
                    (Decode.maybe (columnDecoder name col))
            )
            (Decode.succeed Dict.empty)


columnDecoder : String -> Schema.Column -> Decoder Value
columnDecoder name col =
    (case ( col.constraint, col ) of
        ( ForeignKey ref, _ ) ->
            ref.labelColumnName
                |> Maybe.map
                    (\labelCol ->
                        Decode.field ref.tableName
                            (Decode.map2
                                (\pk label ->
                                    Ref
                                        { tableName = ref.tableName
                                        , primaryKey = pk
                                        , label = label
                                        }
                                )
                                (Decode.field ref.primaryKeyName
                                    (Decode.oneOf
                                        [ Decode.string
                                        , Decode.float |> Decode.map String.fromFloat
                                        ]
                                    )
                                )
                                (Decode.field labelCol Decode.string)
                            )
                    )

        _ ->
            Nothing
    )
        |> Maybe.withDefault (Decode.field name Schema.valueDecoder)


countParser : Parser Count
countParser =
    Parser.succeed
        (\( from, to ) recordCount ->
            Count from to (Maybe.withDefault to recordCount)
        )
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
