module PostgrestAdmin.Config exposing
    ( Config
    , init
    , withHost
    , withFormFields
    , withBasicAuth
    , withJwt
    , withNewResourceMountPoint
    , withResourceMountPoint
    , tableNameParser
    , default
    )

{-| Program configuration

@docs Config


# Init

@docs init


# Basics

@docs withHost
@docs withFormFields


# Auth

@docs withBasicAuth
@docs withJwt


# Element mounting

@docs withNewResourceMountPoint
@docs withResourceMountPoint


# URL parsing

@docs tableNameParser


# Defaults

@docs default

-}

import BasicAuth exposing (BasicAuth)
import Dict exposing (Dict)
import Html exposing (Html)
import Json.Decode as Decode exposing (Decoder)
import Postgrest.Record exposing (Record)
import PostgrestAdmin.AuthScheme as AuthScheme exposing (AuthScheme)
import PostgrestAdmin.Flag as Flag
import PostgrestAdmin.Route exposing (MountPoint(..), Route(..))
import Url exposing (Protocol(..), Url)
import Url.Parser as Parser exposing (Parser)


{-| [PostgrestAdmin.application](PostgrestAdmin#application) configuration
params
-}
type alias Config m msg =
    { host : Url
    , authScheme : AuthScheme
    , formFields : Dict String (List String)
    , resourceRoutes : List (MountPoint m msg)
    }


{-| [PostgrestAdmin.application](PostgrestAdmin#application) decoder with
defaults.

    main : PostgrestAdmin.Program Model Msg
    main =
        PostgrestAdmin.application Config.init

-}
init : Decoder (Config m msg)
init =
    Decode.succeed default
        |> Flag.string "host" withHostDecoder
        |> Flag.stringDict "formFields" withFormFieldsDecoder


{-| Specify the postgREST host.

      main : PostgrestAdmin.Program Never Never
      main =
          Config.init
              |> Config.withHost "http://localhost:3000"
              |> PostgrestAdmin.application

Alternatively the host can be specified using flags, configuring using `withHost`
function precedence.

      Elm.Main.init({
          flags: { host: "http://localhost:3000" }
      })

-}
withHost : String -> Decoder (Config m msg) -> Decoder (Config m msg)
withHost urlStr decoder =
    decoder |> Decode.andThen (withHostDecoder urlStr)


withHostDecoder : String -> Config m msg -> Decoder (Config m msg)
withHostDecoder urlStr conf =
    Url.fromString urlStr
        |> Maybe.map (\u -> Decode.succeed { conf | host = u })
        |> Maybe.withDefault
            (Decode.fail "`Config.withHost` was given an invalid URL")


{-| Enable user credentials form and configure the parameters. Credentials
are be used to obtain a JWT.
-}
withBasicAuth :
    Decoder BasicAuth
    -> Decoder (Config m msg)
    -> Decoder (Config m msg)
withBasicAuth authDecoder decoder =
    Decode.map2 (\auth conf -> { conf | authScheme = AuthScheme.basic auth })
        authDecoder
        decoder


{-| Set a JWT to authenticate postgREST requests.

      main : PostgrestAdmin.Program Never Never
      main =
          Config.init
              |> Config.withJwt "8abf3a...9ac36d"
              |> PostgrestAdmin.application

Alternatively the token can be passed using flags, configuring using `withJwt`
function precedence.

      Elm.Main.init({
          flags: { jwt: sessionStorage.getItem("jwt") }
      })

-}
withJwt : String -> Decoder (Config m msg) -> Decoder (Config m msg)
withJwt tokenStr decoder =
    decoder
        |> Decode.andThen
            (\conf ->
                Decode.succeed { conf | authScheme = AuthScheme.jwt tokenStr }
            )


{-| Specify which fields should be present in the the edit and create forms,
overriding the table schema. By default a primary key field is not present in
the forms.

      main : PostgrestAdmin.Program Never Never
      main =
          Config.init
              |> Config.withFormFields
                  (Dict.fromList [("posts", ["id", "title", "content"])])
              |> PostgrestAdmin.application

Alternatively this parameter can be configured using flags, configuring using
`withFormFields` function precedence.

      Elm.Main.init({
          flags: { formFields: { posts: [ "id", "title", "content" ]} }
      })

-}
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


{-| Create an HTML element for an **existing record** and match to a url path
using
[Url.Parser](https://package.elm-lang.org/packages/elm/url/latest/Url.Parser).
This is usefull if you want to override an existing resource page or add an
additional details page.

The component specification is similar to the specification for
[Browser.element](https://package.elm-lang.org/packages/elm/browser/latest/Browser#element).

    main : PostgrestAdmin.Program Model Msg
    main =
        Config.init
            |> Config.withResourceMountPoint
                { view = view, update = update, init = init }
                (Config.tableNameParser "posts" </> Parser.string </> s "detail")
            |> PostgrestAdmin.application

-}
withResourceMountPoint :
    { init : Record -> ( m, Cmd msg )
    , view : m -> Html msg
    , update : msg -> m -> ( m, Cmd msg )
    }
    -> Parser (String -> String -> Route m msg) (Route m msg)
    -> Decoder (Config m msg)
    -> Decoder (Config m msg)
withResourceMountPoint program parser =
    Decode.andThen
        (\conf ->
            Decode.succeed
                { conf
                    | resourceRoutes =
                        MountPointResource program parser :: conf.resourceRoutes
                }
        )


{-| Create an HTML element for a **blank record** and match to a url path
using
[Url.Parser](https://package.elm-lang.org/packages/elm/url/latest/Url.Parser).
This is usefull if you want to override the record creation form.

The component specification is similar to the specification for
[Browser.element](https://package.elm-lang.org/packages/elm/browser/latest/Browser#element).

    main : PostgrestAdmin.Program Model Msg
    main =
        Config.init
            |> Config.withNewResourceMountPoint
                { view = view, update = update, init = init }
                (Config.tableNameParser "posts" </> s "new-form")
            |> PostgrestAdmin.application

-}
withNewResourceMountPoint :
    { init : Record -> ( m, Cmd msg )
    , view : m -> Html msg
    , update : msg -> m -> ( m, Cmd msg )
    }
    -> Parser (String -> Route m msg) (Route m msg)
    -> Decoder (Config m msg)
    -> Decoder (Config m msg)
withNewResourceMountPoint program parser =
    Decode.andThen
        (\conf ->
            Decode.succeed
                { conf
                    | resourceRoutes =
                        MountPointNewResource program parser
                            :: conf.resourceRoutes
                }
        )


{-| Parse url segment only if it matches string.

      tableNameParser "posts" </> Parser.string </> s "comments"

-}
tableNameParser : String -> Parser (String -> a) a
tableNameParser tableName =
    Parser.custom "TABLE_NAME" <|
        \segment ->
            if segment == tableName then
                Just segment

            else
                Nothing


{-| Configuration defaults
-}
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
