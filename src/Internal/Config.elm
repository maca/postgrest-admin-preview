module Internal.Config exposing
    ( Config
    , default
    , init
    , withBasicAuth
    , withFormFields
    , withHost
    , withJwt
    , withMountPoint
    , withOnLogin
    )

import Dict exposing (Dict)
import Internal.AuthScheme as AuthScheme exposing (AuthScheme)
import Internal.BasicAuth exposing (BasicAuth)
import Internal.Flag as Flag
import Internal.Msg exposing (Msg)
import Internal.Route exposing (Application, MountPoint(..), Route(..))
import Json.Decode as Decode exposing (Decoder)
import Url exposing (Protocol(..), Url)
import Url.Parser exposing (Parser)


type alias Config m msg =
    { host : Url
    , authScheme : AuthScheme
    , formFields : Dict String (List String)
    , application : Maybe (MountPoint m msg)
    , onLogin : String -> Cmd ()
    }


init : Decoder (Config m msg)
init =
    Decode.succeed default
        |> Flag.string "host" withHostDecoder
        |> Flag.stringDict "formFields" withFormFieldsDecoder


withHost : String -> Decoder (Config m msg) -> Decoder (Config m msg)
withHost urlStr decoder =
    decoder |> Decode.andThen (withHostDecoder urlStr)


withHostDecoder : String -> Config m msg -> Decoder (Config m msg)
withHostDecoder urlStr conf =
    Url.fromString urlStr
        |> Maybe.map (\u -> Decode.succeed { conf | host = u })
        |> Maybe.withDefault
            (Decode.fail "`Config.withHost` was given an invalid URL")


withBasicAuth :
    Decoder BasicAuth
    -> Decoder (Config m msg)
    -> Decoder (Config m msg)
withBasicAuth authDecoder decoder =
    Decode.map2 (\auth conf -> { conf | authScheme = AuthScheme.basic auth })
        authDecoder
        decoder


withJwt : String -> Decoder (Config m msg) -> Decoder (Config m msg)
withJwt tokenStr decoder =
    decoder
        |> Decode.andThen
            (\conf ->
                Decode.succeed { conf | authScheme = AuthScheme.jwt tokenStr }
            )


withOnLogin : (String -> Cmd a) -> Decoder (Config m msg) -> Decoder (Config m msg)
withOnLogin onLogin decoder =
    decoder
        |> Decode.andThen
            (\conf ->
                Decode.succeed
                    { conf | onLogin = onLogin >> Cmd.map (always ()) }
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
    Application m msg
    ->
        Parser
            (() -> ( Route m msg, Cmd (Msg msg) ))
            ( Route m msg, Cmd (Msg msg) )
    -> Decoder (Config m msg)
    -> Decoder (Config m msg)
withMountPoint program parser =
    Decode.andThen
        (\conf ->
            Decode.succeed
                { conf | application = Just (MountPoint program parser) }
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
