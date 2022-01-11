module PostgrestAdmin.Config exposing (Config, default, withJwt, withUrl)

import Json.Decode as Decode exposing (Decoder, Value)
import PostgrestAdmin.AuthScheme as AuthScheme exposing (AuthScheme)
import Result
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


withUrl : String -> Decoder Config -> Decoder Config
withUrl urlStr decoder =
    decoder
        |> Decode.andThen
            (\conf ->
                Url.fromString urlStr
                    |> Maybe.map (\u -> Decode.succeed { conf | url = u })
                    |> Maybe.withDefault
                        (Decode.fail "Postgrest url is not valid")
            )


withJwt : String -> Decoder Config -> Decoder Config
withJwt tokenStr decoder =
    decoder
        |> Decode.andThen
            (\conf ->
                Decode.succeed
                    { conf | authScheme = AuthScheme.fromTokenString tokenStr }
            )
