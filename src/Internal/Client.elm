module Internal.Client exposing
    ( Client, AuthScheme(..), AuthError(..)
    , init
    , toJwtString, getTable
    , isAuthenticated, schemaIsLoaded
    , updateJwt, logout, authFailed
    , authSchemeConfig, basic, jwt, unset, authUrl, authUrlDecoder, encoder, decoder
    , task, authHeader, endpoint, fetchSchema
    , listableColumns, listingSelects
    )

{-|

@docs Client, AuthScheme, AuthError
@docs init
@docs toJwtString, getTable
@docs isAuthenticated, schemaIsLoaded
@docs updateJwt, logout, authFailed
@docs authSchemeConfig, basic, jwt, unset, authUrl, authUrlDecoder, encoder, decoder
@docs task, authHeader, endpoint, fetchSchema
@docs listableColumns, listingSelects

-}

import Dict exposing (Dict)
import Http exposing (header)
import Internal.Http
    exposing
        ( Error(..)
        , handleJsonResponse
        , removeLeadingOrTrailingSlash
        )
import Internal.Schema as Schema
    exposing
        ( Column
        , Constraint(..)
        , Schema
        , Table
        )
import Internal.Value exposing (Value(..))
import Json.Decode as Decode exposing (Decoder)
import Json.Encode as Encode
import Postgrest.Client as PG exposing (Selectable)
import String.Extra as String
import Task exposing (Task)
import Url exposing (Url)



-- AUTH TYPES


type AuthScheme
    = Jwt PG.JWT
    | Unset


type AuthError
    = Forbidden
    | Unauthorized
    | ServerError Int
    | DecodeError Decode.Error
    | NetworkError



-- CLIENT TYPES


type alias Client =
    { host : Url
    , authScheme : AuthScheme
    , schema : Schema
    }



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
authUrlDecoder urlStr authScheme =
    Decode.succeed authScheme


encoder : (Dict String String -> Encode.Value) -> Decoder AuthScheme -> Decoder AuthScheme
encoder authEncoder =
    Decode.map identity


decoder : Decoder String -> Decoder AuthScheme -> Decoder AuthScheme
decoder jwtDecoder =
    Decode.map identity



-- CLIENT


init : Url -> AuthScheme -> Client
init url authScheme =
    { host = url
    , authScheme = authScheme
    , schema = Dict.empty
    }


isAuthenticated : Client -> Bool
isAuthenticated { authScheme } =
    authSchemeIsAuthenticated authScheme


authSchemeIsAuthenticated : AuthScheme -> Bool
authSchemeIsAuthenticated authScheme =
    toJwt authScheme |> Maybe.map (always True) |> Maybe.withDefault False


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


getTable : String -> Client -> Maybe Table
getTable tableName { schema } =
    Dict.get tableName schema


updateJwt : String -> Client -> Client
updateJwt tokenStr client =
    { client | authScheme = updateAuthSchemeJwt tokenStr client.authScheme }


updateAuthSchemeJwt : String -> AuthScheme -> AuthScheme
updateAuthSchemeJwt tokenStr authScheme =
    Jwt (PG.jwt tokenStr)


clearJwt : Client -> Client
clearJwt client =
    { client | authScheme = clearAuthSchemeJwt client.authScheme }


clearAuthSchemeJwt : AuthScheme -> AuthScheme
clearAuthSchemeJwt authScheme =
    Unset


logout : Client -> Client
logout client =
    { client
        | authScheme = clearAuthSchemeJwt client.authScheme
        , schema = Dict.empty
    }



fetchSchema :
    { a | tables : List String, tableAliases : Dict String String }
    -> Client
    -> Task Error Schema
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



-- HTTP


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
            fail AuthError


authHeader : Client -> Maybe Http.Header
authHeader client =
    toJwtString client
        |> Maybe.map (\jwtStr -> header "Authorization" ("Bearer " ++ jwtStr))


fail : Error -> Task Error a
fail err =
    case err of
        AuthError ->
            Task.fail AuthError

        _ ->
            Task.fail err



-- HTTP UTILS


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
