module PostgrestAdmin.Config exposing
    ( Config
    , default
    , init
    , withBasicAuth
    , withFormFields
    , withHost
    , withJwt
    )

import BasicAuth exposing (BasicAuth)
import Dict exposing (Dict)
import Html exposing (Html)
import Json.Decode as Decode exposing (Decoder)
import Postgrest.Record exposing (Record)
import PostgrestAdmin.AuthScheme as AuthScheme exposing (AuthScheme)
import PostgrestAdmin.Flag as Flag
import PostgrestAdmin.Route exposing (Route(..))
import Url exposing (Protocol(..), Url)
import Url.Parser as Parser exposing ((</>), Parser, s)


type alias Config =
    { url : Url
    , authScheme : AuthScheme
    , formFields : Dict String (List String)
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
    , formFields = Dict.empty
    }


init : Decoder Config
init =
    Decode.succeed default
        |> Flag.string "host" withHostDecoder
        |> Flag.stringDict "formFields" withFormFieldsDecoder


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


withFormFields : Dict String (List String) -> Decoder Config -> Decoder Config
withFormFields fields decoder =
    decoder |> Decode.andThen (withFormFieldsDecoder fields)


withFormFieldsDecoder : Dict String (List String) -> Config -> Decoder Config
withFormFieldsDecoder fields conf =
    Decode.succeed { conf | formFields = fields }
