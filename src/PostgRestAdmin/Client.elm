module PostgRestAdmin.Client exposing
    ( Client, init
    , Count
    , fetchRecord, fetchRecords, saveRecord, deleteRecord, count, fetchParentLabel
    , jsonRequest, bytesRequest, chunk
    , Error(..), errorToString
    , AuthScheme(..), jwt, unset, toJwtString, authHeader, updateJwt, logout
    , fetchSchema, schemaIsLoaded
    , associationJoin
    , jsonResolver, resetToken, task
    )

{-| PostgREST client for Elm applications.


# Client

@docs Client, init


# Requests

@docs Count
@docs fetchRecord, fetchRecords, saveRecord, deleteRecord, count, fetchParentLabel
@docs jsonRequest, bytesRequest, chunk


# Errors

@docs Error, errorToString


# Auth

@docs AuthScheme, jwt, unset, toJwtString, authHeader, updateJwt, logout


# Schema

@docs fetchSchema, schemaIsLoaded
@docs associationJoin

-}

import Bytes exposing (Bytes)
import Dict exposing (Dict)
import Http
import Internal.Schema as Schema exposing (Column, Table, Value(..))
import Json.Decode as Decode exposing (Decoder)
import Json.Encode as Encode
import Parser exposing ((|.), (|=), Parser)
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
    | BadStatus Int (Maybe String)
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
    , tables : List String
    , tableAliases : Dict String String
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


jwt : String -> AuthScheme
jwt tokenStr =
    Jwt (PG.jwt tokenStr)


unset : AuthScheme
unset =
    Unset


resetToken : Client -> Client
resetToken client =
    { client | authScheme = Unset }



-- CLIENT FUNCTIONS


init :
    { host : Url
    , authScheme : AuthScheme
    , headers : List Http.Header
    , tables : List String
    , tableAliases : Dict String String
    }
    -> Client
init params =
    { host = params.host
    , authScheme = params.authScheme
    , schema = Dict.empty
    , headers = params.headers
    , tables = params.tables
    , tableAliases = params.tableAliases
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


fetchSchema : Client -> Task Error Schema.Schema
fetchSchema client =
    task
        { client = client
        , method = "GET"
        , headers = []
        , path = "/"
        , body = Http.emptyBody
        , resolver =
            jsonResolver
                (Schema.decoder
                    { tables = client.tables
                    , tableAliases = client.tableAliases
                    }
                )
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

        BadStatus status body ->
            "The server responded with an error: "
                ++ (body |> Maybe.withDefault (String.fromInt status))

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
            (\body ->
                Decode.decodeString badStatusDecoder body
                    |> Result.withDefault Nothing
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
            (\body ->
                Decode.decodeString badStatusDecoder body
                    |> Result.withDefault Nothing
            )
        )


resolve :
    (Http.Metadata -> body -> Result Error value)
    -> (body -> Maybe String)
    -> Http.Response body
    -> Result Error value
resolve fn parseError response =
    case response of
        Http.BadUrl_ url ->
            Err (BadUrl url)

        Http.Timeout_ ->
            Err Timeout

        Http.NetworkError_ ->
            Err NetworkError

        Http.BadStatus_ { statusCode } body ->
            case statusCode of
                401 ->
                    Err Unauthorized

                403 ->
                    Err Forbidden

                _ ->
                    Err (BadStatus statusCode (parseError body))

        Http.GoodStatus_ metadata body ->
            fn metadata body


badStatusDecoder : Decoder (Maybe String)
badStatusDecoder =
    Decode.map4
        (\code details hint message ->
            List.filterMap identity
                [ code
                , details
                , hint
                , message
                ]
                |> String.join "\n"
                |> Just
        )
        (Decode.maybe (Decode.field "code" Decode.string))
        (Decode.maybe (Decode.field "details" Decode.string))
        (Decode.maybe (Decode.field "hint" Decode.string))
        (Decode.maybe (Decode.field "message" Decode.string))


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
    }
    -> Task.Task Error a
saveRecord { client, body, table, decoder, id } =
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
    }
    -> Task.Task Error ()
deleteRecord { client, table, id } =
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
jsonRequest :
    { client : Client
    , method : String
    , headers : List Http.Header
    , path : String
    , body : Http.Body
    , decoder : Decoder a
    }
    -> Task Error a
jsonRequest params =
    task
        { client = params.client
        , method = params.method
        , headers = params.headers
        , path = params.path
        , body = params.body
        , resolver = jsonResolver params.decoder
        }


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


chunk : Client -> Table -> List String -> Task.Task Error (List String)
chunk client table ids =
    let
        primaryKeyName =
            Schema.tablePrimaryKeyName table

        queryStr =
            PG.toQueryString
                [ PG.or
                    (List.filterMap
                        (\id ->
                            primaryKeyName
                                |> Maybe.map (\key -> PG.param key (PG.eq (PG.string id)))
                        )
                        ids
                    )
                , PG.select
                    (List.filterMap identity [ Maybe.map PG.attribute primaryKeyName ])
                ]
    in
    task
        { client = client
        , method = "GET"
        , headers = []
        , path = "/" ++ table.name ++ "?" ++ queryStr
        , body = Http.emptyBody
        , resolver =
            jsonResolver
                (case primaryKeyName of
                    Just key ->
                        Decode.list (Decode.field key idDecoder)

                    Nothing ->
                        Decode.fail ""
                )
        }


idDecoder : Decoder String
idDecoder =
    Decode.oneOf
        [ Decode.string
        , Decode.float |> Decode.map String.fromFloat
        ]


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
        , resolver = Http.bytesResolver (resolve (always Ok) (always Nothing))
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
                    (always Nothing)
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
associationJoin { foreignKey } =
    case foreignKey of
        Just fk ->
            fk.labelColumn
                |> Maybe.map
                    (\n ->
                        PG.resource fk.tableName
                            (PG.attributes [ n, fk.foreignKey ])
                    )

        Nothing ->
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
    case col.foreignKey of
        Just ref ->
            Decode.oneOf
                [ Decode.field ref.tableName
                    (Decode.map2 Tuple.pair
                        (ref.labelColumn
                            |> Maybe.map (\c -> Decode.maybe (Decode.field c Decode.string))
                            |> Maybe.withDefault (Decode.succeed Nothing)
                        )
                        (Decode.field ref.foreignKey idDecoder)
                    )
                , Decode.field name idDecoder |> Decode.map (Tuple.pair Nothing)
                ]
                |> Decode.map
                    (\( label, id ) ->
                        Ref { tableName = ref.tableName, primaryKey = id, label = label }
                    )

        Nothing ->
            Decode.field name Schema.valueDecoder


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
