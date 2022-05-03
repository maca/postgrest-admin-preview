module PostgrestAdmin.Config exposing
    ( Config
    , default
    , init
    , withBasicAuth
    , withHost
    , withJwt
    )

import BasicAuth exposing (BasicAuth)
import Json.Decode as Decode exposing (Decoder)
import PostgrestAdmin.AuthScheme as AuthScheme exposing (AuthScheme)
import PostgrestAdmin.ConfigUtils exposing (optionalFlag)
import Url exposing (Protocol(..), Url)


type alias Config =
    { url : Url
    , authScheme : AuthScheme
    }


default : Config
default =
    { authScheme = AuthScheme.unset
    , url =
        { protocol = Http
        , host = "localhost"
        , port_ = Just 3000
        , path = ""
        , query = Nothing
        , fragment = Nothing
        }
    }


init : Decoder Config
init =
    Decode.succeed default
        |> optionalFlag "host" withHostDecoder


withHost : String -> Decoder Config -> Decoder Config
withHost urlStr decoder =
    decoder |> Decode.andThen (withHostDecoder urlStr)


withHostDecoder : String -> Config -> Decoder Config
withHostDecoder urlStr conf =
    Url.fromString urlStr
        |> Maybe.map (\u -> Decode.succeed { conf | url = u })
        |> Maybe.withDefault
            (Decode.fail "`Config.withHost` was given an invalid URL")


withJwt : String -> Decoder Config -> Decoder Config
withJwt tokenStr decoder =
    decoder
        |> Decode.andThen
            (\conf ->
                Decode.succeed { conf | authScheme = AuthScheme.jwt tokenStr }
            )


withBasicAuth : Decoder BasicAuth -> Decoder Config -> Decoder Config
withBasicAuth authDecoder decoder =
    Decode.map2 (\auth conf -> { conf | authScheme = AuthScheme.basic auth })
        authDecoder
        decoder
