module Internal.Config exposing
    ( Config
    , DetailActions
    , default
    , detailActions
    , flagsDecoder
    , formAuth
    , formFields
    , formFieldsDecoder
    , host
    , hostDecoder
    , init
    , jwt
    , mountPath
    , mountPathDecoder
    , onAuthFailed
    , onExternalLogin
    , onLogin
    , onLogout
    , routes
    , tableAliases
    , tableAliasesDecoder
    , tables
    , tablesDecoder
    )

import Dict exposing (Dict)
import Internal.Application as Application
import Internal.AuthScheme as AuthScheme exposing (AuthScheme)
import Internal.Flag as Flag
import Internal.FormAuth as FormAuth exposing (FormAuth)
import Json.Decode as Decode exposing (Decoder)
import PostgRestAdmin.MountPath as MountPath exposing (MountPath)
import PostgRestAdmin.Record exposing (Record)
import Url exposing (Protocol(..), Url)
import Url.Parser exposing (Parser)


type alias DetailActions =
    List ( String, Record -> String -> String )


type alias Login =
    { path : String
    , accessToken : String
    }


type alias Config flags model msg =
    { host : Url
    , mountPath : MountPath
    , authScheme : AuthScheme
    , formFields : Dict String (List String)
    , application :
        Maybe
            ( Application.Params flags model msg
            , Parser (msg -> msg) msg
            )
    , detailActions : Dict String DetailActions
    , tables : List String
    , onLogin : String -> Cmd msg
    , onAuthFailed : String -> Cmd msg
    , onExternalLogin : (Login -> Login) -> Sub Login
    , onLogout : () -> Cmd msg
    , tableAliases : Dict String String
    , flagsDecoder : Decoder flags
    }


init : Decoder (Config f m msg)
init =
    Decode.succeed default


host : String -> Decoder (Config f m msg) -> Decoder (Config f m msg)
host urlStr =
    Decode.andThen (hostDecoder urlStr)


hostDecoder : String -> Config f m msg -> Decoder (Config f m msg)
hostDecoder urlStr conf =
    Url.fromString urlStr
        |> Maybe.map (\u -> Decode.succeed { conf | host = u })
        |> Maybe.withDefault
            (Decode.fail "`Config.host` was given an invalid URL")


mountPath : String -> Decoder (Config f m msg) -> Decoder (Config f m msg)
mountPath p =
    Decode.andThen (mountPathDecoder p)


mountPathDecoder : String -> Config f m msg -> Decoder (Config f m msg)
mountPathDecoder p conf =
    Decode.succeed { conf | mountPath = MountPath.fromString p }


formAuth :
    Decoder FormAuth
    -> Decoder (Config f m msg)
    -> Decoder (Config f m msg)
formAuth authDecoder =
    Decode.map2 (\auth conf -> { conf | authScheme = AuthScheme.basic auth })
        (Flag.string "authUrl" FormAuth.authUrlDecoder authDecoder)


jwt : String -> Decoder (Config f m msg) -> Decoder (Config f m msg)
jwt tokenStr =
    Decode.andThen
        (\conf ->
            Decode.succeed { conf | authScheme = AuthScheme.jwt tokenStr }
        )


onLogin :
    (String -> Cmd msg)
    -> Decoder (Config f m msg)
    -> Decoder (Config f m msg)
onLogin f =
    Decode.andThen
        (\conf -> Decode.succeed { conf | onLogin = f })


onLogout :
    (() -> Cmd msg)
    -> Decoder (Config f m msg)
    -> Decoder (Config f m msg)
onLogout f =
    Decode.andThen
        (\conf -> Decode.succeed { conf | onLogout = f })


onAuthFailed :
    (String -> Cmd msg)
    -> Decoder (Config f m msg)
    -> Decoder (Config f m msg)
onAuthFailed f =
    Decode.andThen
        (\conf -> Decode.succeed { conf | onAuthFailed = f })


onExternalLogin :
    ((Login -> Login) -> Sub Login)
    -> Decoder (Config f m msg)
    -> Decoder (Config f m msg)
onExternalLogin sub =
    Decode.andThen
        (\conf -> Decode.succeed { conf | onExternalLogin = sub })


formFields :
    String
    -> List String
    -> Decoder (Config f m msg)
    -> Decoder (Config f m msg)
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
    -> Config f m msg
    -> Decoder (Config f m msg)
formFieldsDecoder fields conf =
    Decode.succeed { conf | formFields = Dict.union fields conf.formFields }


tableAliases :
    Dict String String
    -> Decoder (Config f m msg)
    -> Decoder (Config f m msg)
tableAliases aliases =
    Decode.andThen (\conf -> Decode.succeed { conf | tableAliases = aliases })


tableAliasesDecoder :
    Dict String String
    -> Config f m msg
    -> Decoder (Config f m msg)
tableAliasesDecoder aliases conf =
    Decode.succeed { conf | tableAliases = aliases }


detailActions :
    String
    -> DetailActions
    -> Decoder (Config f m msg)
    -> Decoder (Config f m msg)
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
    Application.Params f m msg
    -> Parser (msg -> msg) msg
    -> Decoder (Config f m msg)
    -> Decoder (Config f m msg)
routes program parser =
    Decode.andThen
        (\conf ->
            Decode.succeed { conf | application = Just ( program, parser ) }
        )


flagsDecoder :
    Decoder f
    -> Decoder (Config f m msg)
    -> Decoder (Config f m msg)
flagsDecoder decoder =
    Decode.andThen (\conf -> Decode.succeed { conf | flagsDecoder = decoder })


tables : List String -> Decoder (Config f m msg) -> Decoder (Config f m msg)
tables tableNames =
    Decode.andThen (tablesDecoder tableNames)


tablesDecoder : List String -> Config f m msg -> Decoder (Config f m msg)
tablesDecoder tableNames conf =
    Decode.succeed { conf | tables = tableNames }


default : Config f m msg
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
    , mountPath = MountPath.fromString ""
    , formFields = Dict.empty
    , application = Nothing
    , detailActions = Dict.empty
    , tables = []
    , onLogin = always Cmd.none
    , onAuthFailed = always Cmd.none
    , onExternalLogin = always Sub.none
    , onLogout = always Cmd.none
    , tableAliases = Dict.empty
    , flagsDecoder = Decode.fail "No flags decoder provided"
    }
