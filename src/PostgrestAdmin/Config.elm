module PostgrestAdmin.Config exposing
    ( Config
    , default
    , init
    , withBasicAuth
    , withFormFields
    , withHost
    , withJwt
    , withMountedResource
    )

import BasicAuth exposing (BasicAuth)
import Dict exposing (Dict)
import Html exposing (Html)
import Json.Decode as Decode exposing (Decoder)
import Postgrest.Record exposing (Record)
import PostgrestAdmin.AuthScheme as AuthScheme exposing (AuthScheme)
import PostgrestAdmin.Flag as Flag
import PostgrestAdmin.Route exposing (MountPoint, Route(..))
import Url exposing (Protocol(..), Url)
import Url.Parser exposing (Parser)


type alias Config m msg =
    { host : Url
    , authScheme : AuthScheme
    , formFields : Dict String (List String)
    , resourceRoutes : List (MountPoint m msg)
    }


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
    , resourceRoutes = []
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


withJwt : String -> Decoder (Config m msg) -> Decoder (Config m msg)
withJwt tokenStr decoder =
    decoder
        |> Decode.andThen
            (\conf ->
                Decode.succeed { conf | authScheme = AuthScheme.jwt tokenStr }
            )


withBasicAuth :
    Decoder BasicAuth
    -> Decoder (Config m msg)
    -> Decoder (Config m msg)
withBasicAuth authDecoder decoder =
    Decode.map2 (\auth conf -> { conf | authScheme = AuthScheme.basic auth })
        authDecoder
        decoder


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


withMountedResource :
    { init : Record -> ( m, Cmd msg )
    , view : m -> Html msg
    , update : msg -> m -> ( m, Cmd msg )
    }
    -> Parser (String -> String -> Route m msg) (Route m msg)
    -> Decoder (Config m msg)
    -> Decoder (Config m msg)
withMountedResource program parser =
    Decode.andThen
        (\conf ->
            Decode.succeed
                { conf
                    | resourceRoutes =
                        MountPoint program parser :: conf.resourceRoutes
                }
        )
