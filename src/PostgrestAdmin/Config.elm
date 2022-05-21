module PostgrestAdmin.Config exposing
    ( Config
    , init
    , withHost
    , withFormFields
    , withBasicAuth
    , withJwt
    , withMountPoint
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
@docs withMountPoint


# URL parsing

@docs tableNameParser

-}

import Dict exposing (Dict)
import Html exposing (Html)
import Internal.Cmd as AppCmd
import Internal.Config as Config
import Internal.Msg exposing (Msg)
import Internal.Route exposing (MountPoint(..), Route(..))
import Json.Decode exposing (Decoder)
import PostgrestAdmin.BasicAuthConfig exposing (BasicAuthConfig)
import PostgrestAdmin.Client exposing (Client)
import Url exposing (Protocol(..))
import Url.Parser exposing (Parser)


{-| [PostgrestAdmin.application](PostgrestAdmin#application) configuration
params.
-}
type alias Config m msg =
    Decoder (Config.Config m msg)


{-| [PostgrestAdmin.application](PostgrestAdmin#application) decoder with
defaults.

    main : PostgrestAdmin.Program Model Msg
    main =
        PostgrestAdmin.application Config.init

-}
init : Config m msg
init =
    Config.init


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
withHost : String -> Config m msg -> Config m msg
withHost =
    Config.withHost


{-| Enable user credentials form and configure the parameters. Credentials
are be used to obtain a JWT.
-}
withBasicAuth : BasicAuthConfig -> Config m msg -> Config m msg
withBasicAuth =
    Config.withBasicAuth


{-| Set a JWT to authenticate postgREST requests. Even when using basic
authentication it's possible to set an initial JWT.

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
withJwt : String -> Config m msg -> Config m msg
withJwt =
    Config.withJwt


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
withFormFields : Dict String (List String) -> Config m msg -> Config m msg
withFormFields =
    Config.withFormFields


{-| Create an HTML element and match to a url path
using
[Url.Parser](https://package.elm-lang.org/packages/elm/url/latest/Url.Parser).
This is usefull if you want to override an existing resource page or add an
additional details page.

The component specification is similar to the specification for
[Browser.element](https://package.elm-lang.org/packages/elm/browser/latest/Browser#element).

    main : PostgrestAdmin.Program Model Msg
    main =
        Config.init
            |> Config.withMountPoint
                { view = view, update = update, init = init }
                (Config.tableNameParser "posts" </> Parser.string </> s "detail")
            |> PostgrestAdmin.application

-}
withMountPoint :
    { init : Client -> ( m, AppCmd.Cmd msg )
    , view : m -> Html msg
    , update : msg -> m -> ( m, AppCmd.Cmd msg )
    , onLogin : Client -> msg
    }
    ->
        Parser
            (() -> ( Route m msg, Cmd (Msg msg) ))
            ( Route m msg, Cmd (Msg msg) )
    -> Config m msg
    -> Config m msg
withMountPoint =
    Config.withMountPoint
