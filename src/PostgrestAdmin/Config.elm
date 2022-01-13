module PostgrestAdmin.Config exposing
    ( Config
    , default
    , noFlags
    , withJwt
    , withUrl
    )

import Json.Decode as Decode exposing (Decoder)
import PostgrestAdmin.AuthScheme as AuthScheme exposing (AuthScheme)
import Url exposing (Protocol(..), Url)


type alias Config =
    { url : Url
    , authScheme : AuthScheme
    }


default : Config
default =
    { url =
        { protocol = Http
        , host = "localhost"
        , port_ = Just 3000
        , path = ""
        , query = Nothing
        , fragment = Nothing
        }
    , authScheme = AuthScheme.unset
    }


noFlags : Decoder Config
noFlags =
    Decode.succeed default


withUrl : String -> Decoder Config -> Decoder Config
withUrl urlStr decoder =
    decoder |> Decode.andThen (withUrlHelp urlStr)


withUrlHelp : String -> Config -> Decoder Config
withUrlHelp urlStr conf =
    Url.fromString urlStr
        |> Maybe.map (\u -> Decode.succeed { conf | url = u })
        |> Maybe.withDefault
            (Decode.fail "`Config.withUrl` was given an invalid URL")


withJwt : String -> Decoder Config -> Decoder Config
withJwt tokenStr decoder =
    decoder
        |> Decode.andThen
            (\conf ->
                Decode.succeed
                    { conf | authScheme = AuthScheme.fromTokenString tokenStr }
            )
