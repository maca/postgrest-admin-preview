module Internal.Config exposing
    ( Config
    , DetailActions
    , default
    , init
    , withDetailActions
    , withFormAuth
    , withFormFields
    , withFormFieldsDecoder
    , withHost
    , withHostDecoder
    , withJwt
    , withMountPoint
    , withOnAuthFailed
    , withOnExternalLogin
    , withOnLogin
    , withOnLogout
    , withTables
    , withTablesDecoder
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


withHost : String -> Decoder (Config m msg) -> Decoder (Config m msg)
withHost urlStr =
    Decode.andThen (withHostDecoder urlStr)


withHostDecoder : String -> Config m msg -> Decoder (Config m msg)
withHostDecoder urlStr conf =
    Url.fromString urlStr
        |> Maybe.map (\u -> Decode.succeed { conf | host = u })
        |> Maybe.withDefault
            (Decode.fail "`Config.withHost` was given an invalid URL")


withFormAuth :
    Decoder FormAuth
    -> Decoder (Config m msg)
    -> Decoder (Config m msg)
withFormAuth authDecoder =
    Decode.map2 (\auth conf -> { conf | authScheme = AuthScheme.basic auth })
        (authDecoder
            |> Flag.string "authUrl" FormAuth.withAuthUrlDecoder
        )


withJwt : String -> Decoder (Config m msg) -> Decoder (Config m msg)
withJwt tokenStr =
    Decode.andThen
        (\conf ->
            Decode.succeed { conf | authScheme = AuthScheme.jwt tokenStr }
        )


withOnLogin :
    (String -> Cmd msg)
    -> Decoder (Config m msg)
    -> Decoder (Config m msg)
withOnLogin onLogin =
    Decode.andThen
        (\conf -> Decode.succeed { conf | onLogin = onLogin })


withOnLogout :
    (() -> Cmd msg)
    -> Decoder (Config m msg)
    -> Decoder (Config m msg)
withOnLogout onLogout =
    Decode.andThen
        (\conf -> Decode.succeed { conf | onLogout = onLogout })


withOnAuthFailed :
    (String -> Cmd msg)
    -> Decoder (Config m msg)
    -> Decoder (Config m msg)
withOnAuthFailed onAuthFailed =
    Decode.andThen
        (\conf -> Decode.succeed { conf | onAuthFailed = onAuthFailed })


withOnExternalLogin :
    ((Login -> Login) -> Sub Login)
    -> Decoder (Config m msg)
    -> Decoder (Config m msg)
withOnExternalLogin onExternalLogin =
    Decode.andThen
        (\conf -> Decode.succeed { conf | onExternalLogin = onExternalLogin })


withFormFields :
    String
    -> List String
    -> Decoder (Config m msg)
    -> Decoder (Config m msg)
withFormFields tableName fields =
    Decode.andThen
        (\conf ->
            Decode.succeed
                { conf
                    | formFields = Dict.insert tableName fields conf.formFields
                }
        )


withFormFieldsDecoder :
    Dict String (List String)
    -> Config m msg
    -> Decoder (Config m msg)
withFormFieldsDecoder fields conf =
    Decode.succeed { conf | formFields = Dict.union fields conf.formFields }


withDetailActions :
    String
    -> DetailActions
    -> Decoder (Config m msg)
    -> Decoder (Config m msg)
withDetailActions tableName actions =
    Decode.andThen
        (\conf ->
            Decode.succeed
                { conf
                    | detailActions =
                        Dict.insert tableName actions conf.detailActions
                }
        )


withMountPoint :
    Application.Params m msg
    -> Parser (msg -> msg) msg
    -> Decoder (Config m msg)
    -> Decoder (Config m msg)
withMountPoint program parser =
    Decode.andThen
        (\conf ->
            Decode.succeed { conf | application = Just ( program, parser ) }
        )


withTables : List String -> Decoder (Config m msg) -> Decoder (Config m msg)
withTables tableNames =
    Decode.andThen (withTablesDecoder tableNames)


withTablesDecoder : List String -> Config m msg -> Decoder (Config m msg)
withTablesDecoder tableNames conf =
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
