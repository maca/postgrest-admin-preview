module Internal.Config exposing
    ( Config
    , DetailActions
    , default
    , detailActions
    , formAuth
    , formFields
    , formFieldsDecoder
    , host
    , hostDecoder
    , init
    , jwt
    , onAuthFailed
    , onExternalLogin
    , onLogin
    , onLogout
    , routes
    , tables
    , tablesDecoder
    )

import Dict exposing (Dict)
import Internal.Application as Application
import Internal.AuthScheme as AuthScheme exposing (AuthScheme)
import Internal.Flag as Flag
import Internal.FormAuth as FormAuth exposing (FormAuth)
import Json.Decode as Decode exposing (Decoder)
import PostgRestAdmin.Record exposing (Record)
import Url exposing (Protocol(..), Url)
import Url.Parser exposing (Parser)


type alias DetailActions =
    List ( String, Record -> String -> String )


type alias Login =
    { path : String
    , accessToken : String
    }


type alias Config m msg =
    { host : Url
    , authScheme : AuthScheme
    , formFields : Dict String (List String)
    , application :
        Maybe
            ( Application.Params m msg
            , Parser (msg -> msg) msg
            )
    , detailActions : Dict String DetailActions
    , tables : List String
    , onLogin : String -> Cmd msg
    , onAuthFailed : String -> Cmd msg
    , onExternalLogin : (Login -> Login) -> Sub Login
    , onLogout : () -> Cmd msg
    }


init : Decoder (Config m msg)
init =
    Decode.succeed default


host : String -> Decoder (Config m msg) -> Decoder (Config m msg)
host urlStr =
    Decode.andThen (hostDecoder urlStr)


hostDecoder : String -> Config m msg -> Decoder (Config m msg)
hostDecoder urlStr conf =
    Url.fromString urlStr
        |> Maybe.map (\u -> Decode.succeed { conf | host = u })
        |> Maybe.withDefault
            (Decode.fail "`Config.host` was given an invalid URL")


formAuth :
    Decoder FormAuth
    -> Decoder (Config m msg)
    -> Decoder (Config m msg)
formAuth authDecoder =
    Decode.map2 (\auth conf -> { conf | authScheme = AuthScheme.basic auth })
        (authDecoder
            |> Flag.string "authUrl" FormAuth.authUrlDecoder
        )


jwt : String -> Decoder (Config m msg) -> Decoder (Config m msg)
jwt tokenStr =
    Decode.andThen
        (\conf ->
            Decode.succeed { conf | authScheme = AuthScheme.jwt tokenStr }
        )


onLogin :
    (String -> Cmd msg)
    -> Decoder (Config m msg)
    -> Decoder (Config m msg)
onLogin f =
    Decode.andThen
        (\conf -> Decode.succeed { conf | onLogin = f })


onLogout :
    (() -> Cmd msg)
    -> Decoder (Config m msg)
    -> Decoder (Config m msg)
onLogout f =
    Decode.andThen
        (\conf -> Decode.succeed { conf | onLogout = f })


onAuthFailed :
    (String -> Cmd msg)
    -> Decoder (Config m msg)
    -> Decoder (Config m msg)
onAuthFailed f =
    Decode.andThen
        (\conf -> Decode.succeed { conf | onAuthFailed = f })


onExternalLogin :
    ((Login -> Login) -> Sub Login)
    -> Decoder (Config m msg)
    -> Decoder (Config m msg)
onExternalLogin sub =
    Decode.andThen
        (\conf -> Decode.succeed { conf | onExternalLogin = sub })


formFields :
    String
    -> List String
    -> Decoder (Config m msg)
    -> Decoder (Config m msg)
formFields tableName fields =
    Decode.andThen
        (\conf ->
            Decode.succeed
                { conf
                    | formFields = Dict.insert tableName fields conf.formFields
                }
        )


formFieldsDecoder :
    Dict String (List String)
    -> Config m msg
    -> Decoder (Config m msg)
formFieldsDecoder fields conf =
    Decode.succeed { conf | formFields = Dict.union fields conf.formFields }


detailActions :
    String
    -> DetailActions
    -> Decoder (Config m msg)
    -> Decoder (Config m msg)
detailActions tableName actions =
    Decode.andThen
        (\conf ->
            Decode.succeed
                { conf
                    | detailActions =
                        Dict.insert tableName actions conf.detailActions
                }
        )


routes :
    Application.Params m msg
    -> Parser (msg -> msg) msg
    -> Decoder (Config m msg)
    -> Decoder (Config m msg)
routes program parser =
    Decode.andThen
        (\conf ->
            Decode.succeed { conf | application = Just ( program, parser ) }
        )


tables : List String -> Decoder (Config m msg) -> Decoder (Config m msg)
tables tableNames =
    Decode.andThen (tablesDecoder tableNames)


tablesDecoder : List String -> Config m msg -> Decoder (Config m msg)
tablesDecoder tableNames conf =
    Decode.succeed { conf | tables = tableNames }


default : Config m msg
default =
    { authScheme = AuthScheme.unset
    , host =
        { protocol = Http
        , host = "localhost"
        , port_ = Just 3000
        , path = ""
        , query = Nothing
        , fragment = Nothing
        }
    , formFields = Dict.empty
    , application = Nothing
    , detailActions = Dict.empty
    , tables = []
    , onLogin = always Cmd.none
    , onAuthFailed = always Cmd.none
    , onExternalLogin = always Sub.none
    , onLogout = always Cmd.none
    }
