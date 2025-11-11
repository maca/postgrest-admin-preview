module PostgRestAdmin.Client exposing
    ( Client, init
    , fetchRecord, saveRecord, deleteRecord, request, requestMany, Count
    , Error(..), errorToString
    , AuthScheme(..), jwt, unset, toJwtString, authHeader, updateJwt, logout
    , fetchSchema, schemaIsLoaded
    , endpoint
    , authSchemeConfig, authUrl, authUrlDecoder, basic, encoder, jwtDecoder
    , bytesRequest, task
    )

{-| PostgREST client for Elm applications.


# Client

@docs Client, init


# Requests

Note that the request functions **do not produce a vanilla Elm
[Cmd](https://package.elm-lang.org/packages/elm/core/latest/Platform-Cmd#Cmd)**
but a [PostgRestAdmin.Cmd](PostgRestAdmin-Cmd).

@docs fetchRecord, fetchRecordList, saveRecord, deleteRecord, request, requestMany, Collection, Count


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
        , resolver = oneResolver (Schema.decoder config)
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


oneResolver : Decode.Decoder a -> Http.Resolver Error a
oneResolver decoder =
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
                    Just (Ok count) ->
                        Decode.decodeString
                            (decoder |> Decode.map (\a -> ( a, count )))
                            body
                            |> Result.mapError DecodeError

                    _ ->
                        Err (BadHeader "content-range")
            )
        )


bytesResolver : Http.Resolver Error Bytes
bytesResolver =
    Http.bytesResolver (resolve (\_ body -> Ok body))


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
                , url = endpoint client ("/" ++ table.name ++ "?" ++ queryStr)
                , body = Http.emptyBody
                , resolver = oneResolver decoder
                , timeout = Nothing
                }



-- Internal.Fetch expect missingPrimaryKey


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
                , resolver = oneResolver decoder
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
                    Http.task
                        { method = "DELETE"
                        , headers = List.filterMap identity [ authHeader client ]
                        , url = endpoint client path
                        , body = Http.emptyBody
                        , resolver = oneResolver (Decode.succeed ())
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
request params =
    task params
        |> Task.attempt params.expect
        |> AppCmd.wrap


task :
    { a
        | client : Client
        , method : String
        , headers : List Http.Header
        , path : String
        , body : Http.Body
        , decoder : Decoder b
    }
    -> Task Error b
task { client, method, headers, path, body, decoder } =
    Http.task
        { method = method
        , headers =
            List.concat
                [ authHeader client
                    |> Maybe.map List.singleton
                    |> Maybe.withDefault []
                , headers
                ]
        , url = endpoint client path
        , body = body
        , resolver = oneResolver decoder
        , timeout = Nothing
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
    Http.task
        { method = method
        , headers =
            List.concat
                [ authHeader client
                    |> Maybe.map List.singleton
                    |> Maybe.withDefault []
                , headers
                ]
        , url = endpoint client path
        , body = body
        , resolver = Http.bytesResolver (resolve (always Ok))
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
    , expect : Result Error ( List a, Count ) -> msg
    }
    -> AppCmd.Cmd msg
requestMany { client, method, headers, path, decoder, body, expect } =
    AppCmd.wrap <|
        Task.attempt expect <|
            Http.task
                { method = method
                , headers = List.filterMap identity (authHeader client :: List.map Just headers)
                , url = endpoint client path
                , body = body
                , resolver = countResolver (Decode.list decoder)
                , timeout = Nothing
                }



-- fetchRecordList :
--     { client : Client
--     , table : Table
--     , queryString : String
--     , expect : Result Error ( List Record, Count ) -> msg
--     }
--     -> AppCmd.Cmd msg
-- fetchRecordList { client, table, queryString, expect } =
--     let
--         selectString =
--             PG.toQueryString [ PG.select (selects table) ]
--     in
--     AppCmd.wrap <|
--         Task.attempt expect <|
--             Http.task
--                 { method = "GET"
--                 , headers =
--                     List.filterMap identity
--                         [ authHeader client
--                         , Just (Http.header "Prefer" "count=planned")
--                         ]
--                 , url = endpoint client ("/" ++ tableName table ++ "?" ++ selectString ++ "&" ++ queryString)
--                 , body = Http.emptyBody
--                 , resolver = countResolver (Decode.list (Record.decoder table))
--                 , timeout = Nothing
--                 }


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


missingPrimaryKey : Task Error a
missingPrimaryKey =
    Task.fail (RequestError "I cound't figure the primary key")


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
