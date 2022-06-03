module Internal.Config exposing
    ( Config
    , MountPoint
    , default
    , init
    , withFormAuth
    , withFormFields
    , withFormFieldsDecoder
    , withHost
    , withHostDecoder
    , withJwt
    , withMountPoint
    , withOnLogin
    )

import Dict exposing (Dict)
import Internal.Application as Application
import Internal.AuthScheme as AuthScheme exposing (AuthScheme)
import Internal.Flag as Flag
import Internal.FormAuth as FormAuth exposing (FormAuth)
import Json.Decode as Decode exposing (Decoder)
import Url exposing (Protocol(..), Url)
import Url.Parser exposing (Parser)


type alias MountPoint m msg =
    ( Application.Params m msg
    , Parser (msg -> msg) msg
    )


type alias Config m msg =
    { host : Url
    , authScheme : AuthScheme
    , formFields : Dict String (List String)
    , application : Maybe (MountPoint m msg)
    , onLogin : String -> Cmd msg
    }


init : Decoder (Config m msg)
init =
    Decode.succeed default


withHost : String -> Decoder (Config m msg) -> Decoder (Config m msg)
withHost urlStr decoder =
    decoder |> Decode.andThen (withHostDecoder urlStr)


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
withFormAuth authDecoder decoder =
    Decode.map2 (\auth conf -> { conf | authScheme = AuthScheme.basic auth })
        (authDecoder
            |> Flag.string "authUrl" FormAuth.withAuthUrlDecoder
        )
        decoder


withJwt : String -> Decoder (Config m msg) -> Decoder (Config m msg)
withJwt tokenStr decoder =
    decoder
        |> Decode.andThen
            (\conf ->
                Decode.succeed { conf | authScheme = AuthScheme.jwt tokenStr }
            )


withOnLogin :
    (String -> Cmd msg)
    -> Decoder (Config m msg)
    -> Decoder (Config m msg)
withOnLogin onLogin decoder =
    decoder
        |> Decode.andThen
            (\conf ->
                Decode.succeed
                    { conf | onLogin = onLogin }
            )


withFormFields :
    Dict String (List String)
    -> Decoder (Config m msg)
    -> Decoder (Config m msg)
withFormFields fields =
    Decode.andThen (withFormFieldsDecoder fields)


withFormFieldsDecoder :
    Dict String (List String)
    -> Config m msg
    -> Decoder (Config m msg)
withFormFieldsDecoder fields conf =
    Decode.succeed { conf | formFields = fields }


withMountPoint :
    Application.Params m msg
    -> Parser (msg -> msg) msg
    -> Decoder (Config m msg)
    -> Decoder (Config m msg)
withMountPoint program parser =
    Decode.andThen
        (\conf ->
            Decode.succeed
                { conf | application = Just ( program, parser ) }
        )


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
    , onLogin = always Cmd.none
    }
