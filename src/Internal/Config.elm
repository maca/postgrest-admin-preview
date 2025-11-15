module Internal.Config exposing
    ( Config
    , DetailActions
    , clientHeaders
    , clientHeadersDecoder
    , decode
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
    , jwtDecoder
    , loginUrl
    , loginUrlDecoder
    , menuLinks
    , menuLinksDecoder
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
import Http
import Internal.Application as Application
import Internal.Flag as Flag
import Internal.Schema exposing (Record)
import Json.Decode as Decode exposing (Decoder)
import PostgRestAdmin.Client as Client exposing (AuthScheme)
import PostgRestAdmin.MountPath as MountPath exposing (MountPath)
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
    , loginUrl : Url
    , authScheme : AuthScheme
    , formFields : Dict String (List String)
    , application : Maybe ( Application.Params flags model msg, Parser (msg -> msg) msg )
    , detailActions : Dict String DetailActions
    , tables : List String
    , menuLinks : List ( String, String )
    , menuActions : Dict String Url
    , onLogin : String -> Cmd msg
    , onAuthFailed : String -> Cmd msg
    , onExternalLogin : (Login -> Login) -> Sub Login
    , onLogout : () -> Cmd msg
    , tableAliases : Dict String String
    , flagsDecoder : Decoder flags
    , clientHeaders : List Http.Header
    }


init : Decoder (Config f m msg)
init =
    Decode.succeed default


decode : Decoder (Config f m msg) -> Decode.Value -> Result Decode.Error (Config f m msg)
decode decoder =
    Decode.decodeValue
        (decoder
            |> Flag.string "host" hostDecoder
            |> Flag.string "loginUrl" loginUrlDecoder
            |> Flag.string "mountPath" mountPathDecoder
            |> Flag.string "jwt" jwtDecoder
            |> Flag.stringListDict "formFields" formFieldsDecoder
            |> Flag.stringList "tables" tablesDecoder
            |> Flag.stringDict "tableAliases" tableAliasesDecoder
            |> Flag.linksList "menuLinks" menuLinksDecoder
            |> Flag.headersList "clientHeaders" clientHeadersDecoder
        )


host : String -> Decoder (Config f m msg) -> Decoder (Config f m msg)
host urlStr =
    Decode.andThen (hostDecoder urlStr)


hostDecoder : String -> Config f m msg -> Decoder (Config f m msg)
hostDecoder urlStr conf =
    Url.fromString urlStr
        |> Maybe.map
            (\u ->
                Decode.succeed
                    { conf
                        | host = u
                        , loginUrl = { u | path = "/rpc/login" }
                    }
            )
        |> Maybe.withDefault
            (Decode.fail "`Config.host` was given an invalid URL")


mountPath : String -> Decoder (Config f m msg) -> Decoder (Config f m msg)
mountPath p =
    Decode.andThen (mountPathDecoder p)


mountPathDecoder : String -> Config f m msg -> Decoder (Config f m msg)
mountPathDecoder p conf =
    Decode.succeed { conf | mountPath = MountPath.fromString p }


loginUrl : String -> Decoder (Config f m msg) -> Decoder (Config f m msg)
loginUrl urlStr =
    Decode.andThen (loginUrlDecoder urlStr)


loginUrlDecoder : String -> Config f m msg -> Decoder (Config f m msg)
loginUrlDecoder urlStr conf =
    Url.fromString urlStr
        |> Maybe.map (\u -> Decode.succeed { conf | loginUrl = u })
        |> Maybe.withDefault
            (Decode.fail "`Config.loginUrl` was given an invalid URL")


formAuth :
    Decoder AuthScheme
    -> Decoder (Config f m msg)
    -> Decoder (Config f m msg)
formAuth authDecoder =
    Decode.map2 (\auth conf -> { conf | authScheme = Client.basic auth })
        (Flag.string "authUrl" Client.authUrlDecoder authDecoder)


jwt : String -> Decoder (Config f m msg) -> Decoder (Config f m msg)
jwt tokenStr =
    Decode.map
        (\conf ->
            { conf | authScheme = Client.jwt tokenStr }
        )


jwtDecoder : String -> Config f m msg -> Decoder (Config f m msg)
jwtDecoder tokenStr conf =
    Decode.succeed { conf | authScheme = Client.jwt tokenStr }


onLogin :
    (String -> Cmd msg)
    -> Decoder (Config f m msg)
    -> Decoder (Config f m msg)
onLogin f =
    Decode.map
        (\conf -> { conf | onLogin = f })


onLogout :
    (() -> Cmd msg)
    -> Decoder (Config f m msg)
    -> Decoder (Config f m msg)
onLogout f =
    Decode.map
        (\conf -> { conf | onLogout = f })


onAuthFailed :
    (String -> Cmd msg)
    -> Decoder (Config f m msg)
    -> Decoder (Config f m msg)
onAuthFailed f =
    Decode.map
        (\conf -> { conf | onAuthFailed = f })


onExternalLogin :
    ((Login -> Login) -> Sub Login)
    -> Decoder (Config f m msg)
    -> Decoder (Config f m msg)
onExternalLogin sub =
    Decode.map
        (\conf -> { conf | onExternalLogin = sub })


formFields :
    String
    -> List String
    -> Decoder (Config f m msg)
    -> Decoder (Config f m msg)
formFields tableName fields =
    Decode.map
        (\conf ->
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
    Decode.map (\conf -> { conf | tableAliases = aliases })


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
    Decode.map
        (\conf ->
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
    Decode.map
        (\conf ->
            { conf | application = Just ( program, parser ) }
        )


flagsDecoder :
    Decoder f
    -> Decoder (Config f m msg)
    -> Decoder (Config f m msg)
flagsDecoder decoder =
    Decode.map (\conf -> { conf | flagsDecoder = decoder })


tables : List String -> Decoder (Config f m msg) -> Decoder (Config f m msg)
tables tableNames =
    Decode.andThen (tablesDecoder tableNames)


tablesDecoder : List String -> Config f m msg -> Decoder (Config f m msg)
tablesDecoder tableNames conf =
    Decode.succeed { conf | tables = tableNames }


menuLinks :
    List ( String, String )
    -> Decoder (Config f m msg)
    -> Decoder (Config f m msg)
menuLinks links =
    Decode.andThen (menuLinksDecoder links)


menuLinksDecoder :
    List ( String, String )
    -> Config f m msg
    -> Decoder (Config f m msg)
menuLinksDecoder links conf =
    Decode.succeed { conf | menuLinks = links }


clientHeaders :
    List Http.Header
    -> Decoder (Config f m msg)
    -> Decoder (Config f m msg)
clientHeaders headers =
    Decode.andThen (clientHeadersDecoder headers)


clientHeadersDecoder :
    List Http.Header
    -> Config f m msg
    -> Decoder (Config f m msg)
clientHeadersDecoder headers conf =
    Decode.succeed { conf | clientHeaders = headers }


default : Config f m msg
default =
    let
        defaultHost =
            { protocol = Http
            , host = "localhost"
            , port_ = Just 3000
            , path = ""
            , query = Nothing
            , fragment = Nothing
            }
    in
    { authScheme = Client.unset
    , host = defaultHost
    , mountPath = MountPath.fromString ""
    , loginUrl = { defaultHost | path = "/rpc/login" }
    , formFields = Dict.empty
    , application = Nothing
    , detailActions = Dict.empty
    , tables = []
    , menuLinks = []
    , menuActions = Dict.empty
    , onLogin = always Cmd.none
    , onAuthFailed = always Cmd.none
    , onExternalLogin = always Sub.none
    , onLogout = always Cmd.none
    , tableAliases = Dict.empty
    , flagsDecoder = Decode.fail "No flags decoder provided"
    , clientHeaders = []
    }
