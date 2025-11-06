module Internal.Client exposing
    ( Client, Msg, Session(..), AuthScheme
    , init
    , toJwtString, getTable
    , isAuthenticated, schemaIsLoaded, isAuthSuccessMsg
    , updateJwt, clearJwt, logout, authFailed
    , authSchemeConfig, basic, jwt, unset, authUrl, authUrlDecoder, encoder, decoder
    , update
    , task, authHeader, endpoint, fetchSchema
    , listableColumns, listingSelects, selects
    , viewAuthForm
    )

{-|

@docs Client, Msg, Session, AuthScheme
@docs init
@docs toJwtString, getTable
@docs isAuthenticated, schemaIsLoaded, isAuthSuccessMsg
@docs updateJwt, clearJwt, logout, authFailed
@docs authSchemeConfig, basic, jwt, unset, authUrl, authUrlDecoder, encoder, decoder
@docs update, viewAuthForm
@docs task, authHeader, endpoint, fetchSchema
@docs listableColumns, listingSelects, selects

-}

import Dict exposing (Dict)
import Dict.Extra as Dict
import FormToolkit.Field as Field
import FormToolkit.Parse as Parse
import Html exposing (Html)
import Html.Attributes as Attrs
import Html.Events as Events
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
import Url exposing (Protocol(..), Url)



-- AUTH TYPES


type Session
    = Token PG.JWT


type AuthScheme
    = FormAuth AuthForm
    | Jwt PG.JWT
    | Unset


type alias AuthForm =
    { url : Url
    , decoder : Decoder Session
    , encoder : Dict String String -> Encode.Value
    , form : Field.Field Never
    , status : AuthFormStatus
    }


type AuthFormStatus
    = Ready
    | Active
    | Success Session
    | Failure AuthError


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


type Msg
    = FieldsChanged (Field.Msg Never)
    | Submitted
    | GotToken (Result AuthError Session)
    | SchemaFetched (Result Error Schema)



-- AUTH CONFIG


authSchemeConfig : Decoder AuthScheme
authSchemeConfig =
    let
        formAuthData =
            { url =
                { protocol = Http
                , host = "localhost"
                , port_ = Just 3000
                , path = "rpc/login"
                , query = Nothing
                , fragment = Nothing
                }
            , decoder =
                Decode.map (PG.jwt >> Token)
                    (Decode.field "token" Decode.string)
            , encoder = Encode.dict identity Encode.string
            , form = form
            , status = Ready
            }
    in
    Decode.oneOf
        [ Decode.field "jwt" Decode.string
            |> Decode.map
                (\token ->
                    FormAuth
                        { formAuthData | status = Success (Token <| PG.jwt token) }
                )
        , Decode.succeed (FormAuth formAuthData)
        ]


form : Field.Field Never
form =
    Field.group []
        [ Field.text
            [ Field.name "email"
            , Field.label "Login"
            , Field.required True
            ]
        , Field.password
            [ Field.name "password"
            , Field.label "Password"
            , Field.required True
            ]
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
    case authScheme of
        FormAuth data ->
            Url.fromString urlStr
                |> Maybe.map
                    (\url -> FormAuth { data | url = url } |> Decode.succeed)
                |> Maybe.withDefault
                    (Decode.fail "`authUrl` was given an invalid URL")

        _ ->
            Decode.succeed authScheme


encoder : (Dict String String -> Encode.Value) -> Decoder AuthScheme -> Decoder AuthScheme
encoder authEncoder =
    Decode.andThen
        (\authScheme ->
            case authScheme of
                FormAuth data ->
                    FormAuth { data | encoder = authEncoder }
                        |> Decode.succeed

                _ ->
                    Decode.succeed authScheme
        )


decoder : Decoder String -> Decoder AuthScheme -> Decoder AuthScheme
decoder jwtDecoder =
    Decode.andThen
        (\authScheme ->
            case authScheme of
                FormAuth data ->
                    FormAuth
                        { data
                            | decoder = Decode.map (PG.jwt >> Token) jwtDecoder
                        }
                        |> Decode.succeed

                _ ->
                    Decode.succeed authScheme
        )



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


isAuthSuccessMsg : Msg -> Bool
isAuthSuccessMsg msg =
    case msg of
        GotToken (Ok _) ->
            True

        _ ->
            False


authFailed : Client -> Client
authFailed client =
    { client | authScheme = failAuthScheme client.authScheme }


failAuthScheme : AuthScheme -> AuthScheme
failAuthScheme authScheme =
    case authScheme of
        FormAuth data ->
            FormAuth { data | status = Failure Unauthorized }

        Jwt _ ->
            Unset

        Unset ->
            Unset


schemaIsLoaded : Client -> Bool
schemaIsLoaded { schema } =
    not (schema == Dict.empty)


toJwtString : Client -> Maybe String
toJwtString { authScheme } =
    toJwt authScheme
        |> Maybe.map PG.jwtString


toJwt : AuthScheme -> Maybe PG.JWT
toJwt authScheme =
    case authScheme of
        FormAuth data ->
            statusToJwt data.status

        Jwt token ->
            Just token

        Unset ->
            Nothing


statusToJwt : AuthFormStatus -> Maybe PG.JWT
statusToJwt status =
    case status of
        Success session ->
            Just (sessionToJwt session)

        _ ->
            Nothing


sessionToJwt : Session -> PG.JWT
sessionToJwt session =
    case session of
        Token token ->
            token


getTable : String -> Client -> Maybe Table
getTable tableName { schema } =
    Dict.get tableName schema


updateJwt : String -> Client -> Client
updateJwt tokenStr client =
    { client | authScheme = updateAuthSchemeJwt tokenStr client.authScheme }


updateAuthSchemeJwt : String -> AuthScheme -> AuthScheme
updateAuthSchemeJwt tokenStr authScheme =
    case authScheme of
        FormAuth data ->
            FormAuth { data | status = Success (Token (PG.jwt tokenStr)) }

        _ ->
            Jwt (PG.jwt tokenStr)


clearJwt : Client -> Client
clearJwt client =
    { client | authScheme = clearAuthSchemeJwt client.authScheme }


clearAuthSchemeJwt : AuthScheme -> AuthScheme
clearAuthSchemeJwt authScheme =
    case authScheme of
        FormAuth data ->
            FormAuth { data | status = Ready }

        _ ->
            Unset


logout : Client -> Client
logout client =
    { client
        | authScheme = clearAuthSchemeJwt client.authScheme
        , schema = Dict.empty
    }



-- UPDATE


update : Msg -> Client -> ( Client, Cmd Msg )
update msg client =
    case msg of
        FieldsChanged innerMsg ->
            case client.authScheme of
                FormAuth data ->
                    ( { client
                        | authScheme =
                            FormAuth
                                { data
                                    | form = Field.update innerMsg data.form
                                    , status = Active
                                }
                      }
                    , Cmd.none
                    )

                _ ->
                    ( client, Cmd.none )

        Submitted ->
            case client.authScheme of
                FormAuth data ->
                    ( client
                    , Task.attempt GotToken (requestToken data)
                    )

                _ ->
                    ( client, Cmd.none )

        GotToken (Ok session) ->
            case client.authScheme of
                FormAuth data ->
                    ( { client
                        | authScheme =
                            FormAuth
                                { data
                                    | form = form
                                    , status = Success session
                                }
                      }
                    , Cmd.none
                    )

                _ ->
                    ( client, Cmd.none )

        GotToken (Err error) ->
            case client.authScheme of
                FormAuth data ->
                    ( { client
                        | authScheme = FormAuth { data | status = Failure error }
                      }
                    , Cmd.none
                    )

                _ ->
                    ( client, Cmd.none )

        SchemaFetched (Ok schema) ->
            ( { client | schema = schema }
            , Cmd.none
            )

        SchemaFetched (Err err) ->
            case err of
                AuthError ->
                    ( { client | authScheme = failAuthScheme client.authScheme }
                    , Cmd.none
                    )

                _ ->
                    ( client, Cmd.none )


requestToken : AuthForm -> Task AuthError Session
requestToken data =
    Http.task
        { method = "POST"
        , headers = []
        , url = Url.toString data.url
        , body =
            Http.jsonBody
                (Parse.parse Parse.json data.form
                    |> Result.withDefault Encode.null
                )
        , resolver = Http.stringResolver (handleAuthResponse data.decoder)
        , timeout = Nothing
        }


handleAuthResponse : Decoder a -> Http.Response String -> Result AuthError a
handleAuthResponse aDecoder response =
    case response of
        Http.BadStatus_ { statusCode } _ ->
            if statusCode == 401 || statusCode == 403 then
                Err Forbidden

            else
                Err (ServerError statusCode)

        Http.GoodStatus_ _ body ->
            case Decode.decodeString aDecoder body of
                Err err ->
                    Err (DecodeError err)

                Ok result ->
                    Ok result

        _ ->
            Err NetworkError


fetchSchema :
    { a | tables : List String, tableAliases : Dict String String }
    -> Client
    -> Cmd Msg
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
        |> Task.attempt SchemaFetched



-- VIEW


viewAuthForm : Client -> Html Msg
viewAuthForm { authScheme } =
    viewAuthScheme authScheme


viewAuthScheme : AuthScheme -> Html Msg
viewAuthScheme authScheme =
    case authScheme of
        FormAuth data ->
            viewFormAuth data

        Unset ->
            Html.div
                [ Attrs.class "overlay overlay-waiting" ]
                [ Html.i [ Attrs.class "gg-spinner-alt" ] []
                ]

        Jwt _ ->
            Html.text ""


viewFormAuth : AuthForm -> Html Msg
viewFormAuth data =
    if requiresAuthentication data.status then
        Html.div
            [ Attrs.class "auth-modal overlay" ]
            [ Html.div
                [ Attrs.class "auth-form" ]
                [ errorMessage data.status
                , Html.form
                    [ Attrs.class "auth-form"
                    , Events.onSubmit Submitted
                    ]
                    [ Field.toHtml FieldsChanged data.form
                    , Html.button
                        [ Attrs.disabled
                            (Parse.parse Parse.json data.form
                                |> Result.map (\_ -> False)
                                |> Result.withDefault True
                            )
                        ]
                        [ Html.text "Login" ]
                    ]
                ]
            ]

    else
        Html.text ""


requiresAuthentication : AuthFormStatus -> Bool
requiresAuthentication status =
    case status of
        Ready ->
            True

        Active ->
            True

        Failure _ ->
            True

        Success _ ->
            False


errorMessage : AuthFormStatus -> Html Msg
errorMessage status =
    case status of
        Failure error ->
            case error of
                Forbidden ->
                    errorWrapper
                        [ Html.text """You may have entered the wrong password,
                          please try again."""
                        ]

                Unauthorized ->
                    errorWrapper
                        [ Html.text
                            "Please sign in to continue."
                        ]

                ServerError statusCode ->
                    errorWrapper
                        [ Html.text "The server responded with an error: "
                        , Html.pre [] [ Html.text (String.fromInt statusCode) ]
                        ]

                DecodeError err ->
                    errorWrapper
                        [ Html.text "There was an issue parsing the server response: "
                        , Html.pre [] [ Html.text (Decode.errorToString err) ]
                        ]

                NetworkError ->
                    errorWrapper
                        [ Html.text """There was an issue reaching the server,
                          please try again later."""
                        ]

        _ ->
            Html.div
                [ Attrs.class "form-error-message"
                , Attrs.style "visibility" "hidden"
                ]
                []


errorWrapper : List (Html Msg) -> Html Msg
errorWrapper =
    Html.div [ Attrs.class "form-error-message" ]



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
