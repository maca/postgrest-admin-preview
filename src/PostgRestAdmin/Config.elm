module PostgRestAdmin.Config exposing
    ( Config
    , init
    , withHost
    , withFormFields
    , withFormAuth
    , withJwt
    , withOnLogin
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

@docs withFormAuth
@docs withJwt
@docs withOnLogin


# Application mounting

@docs withMountPoint

-}

import Dict exposing (Dict)
import Html exposing (Html)
import Internal.Cmd as AppCmd
import Internal.Config as Config
import Internal.Msg exposing (Msg)
import Internal.Route exposing (Route(..))
import Json.Decode exposing (Decoder)
import PostgRestAdmin.Client exposing (Client)
import PostgRestAdmin.Config.FormAuth exposing (FormAuth)
import Url exposing (Protocol(..))
import Url.Parser exposing (Parser)


{-| [PostgRestAdmin.application](PostgRestAdmin#application) configuration
params.
-}
type alias Config m msg =
    Decoder (Config.Config m msg)


{-| [PostgRestAdmin.application](PostgRestAdmin#application) decoder with
defaults.

    main : PostgRestAdmin.Program Never Never
    main =
        PostgRestAdmin.application Config.init

-}
init : Config m msg
init =
    Config.init


{-| Specify the postgREST host.

      main : PostgRestAdmin.Program Never Never
      main =
          Config.init
              |> Config.withHost "http://localhost:3000"
              |> PostgRestAdmin.application

Alternatively the host can be specified using flags, configuring using `withHost`.
Program flags take precedence.

      Elm.Main.init({
          flags: { host: "http://localhost:3000" }
      })

-}
withHost : String -> Config m msg -> Config m msg
withHost =
    Config.withHost


{-| Enable user credentials form and configure the parameters. Credentials
are be used to obtain a JWT.

See [FormAuth](PostgRestAdmin.FormAuth) for configuration options.

      import PostgRestAdmin.Config.FormAuth as FormAuth

      main : PostgRestAdmin.Program Never Never
      main =
          Config.init
              |> Config.withFormAuth FormAuth.config
              |> PostgRestAdmin.application

-}
withFormAuth : FormAuth -> Config m msg -> Config m msg
withFormAuth =
    Config.withFormAuth


{-| Set a JWT to authenticate postgREST requests. Even when using
[withFormAuth](#withFormAuth) it's possible to set an initial JWT.

      main : PostgRestAdmin.Program Never Never
      main =
          Config.init
              |> Config.withJwt "8abf3a...9ac36d"
              |> PostgRestAdmin.application

Alternatively the token can be passed using flags, configuring using `withJwt`.
Program flags take precedence.

      Elm.Main.init({
          flags: { jwt: sessionStorage.getItem("jwt") }
      })

-}
withJwt : String -> Config m msg -> Config m msg
withJwt =
    Config.withJwt


{-| Callback triggered with a JWT string on successful login.
Tipically used to persist the JWT to session storage.

          port loginSuccess : String -> Cmd msg

          main : PostgRestAdmin.Program Never Never
          main =
              Config.init
                  |> Config.withOnLogin loginSuccess
                  |> PostgRestAdmin.application

Elm init

        app = Elm.Main.init({
          flags: { jwt: sessionStorage.getItem("jwt") }
        })

        app.ports.loginSuccess.subscribe(jwt => {
          sessionStorage.setItem("jwt", jwt)
        });

-}
withOnLogin : (String -> Cmd msg) -> Config m msg -> Config m msg
withOnLogin =
    Config.withOnLogin


{-| Specify which fields should be present in the the edit and create forms,
overriding the table schema. By default a primary key field is not present in
the forms.

      main : PostgRestAdmin.Program Never Never
      main =
          Config.init
              |> Config.withFormFields
                  (Dict.fromList
                       [("posts", ["id", "title", "content"])])
              |> PostgRestAdmin.application

Alternatively this parameter can be configured using flags, configuring using
`withFormFields`. Program flags take precedence.

      Elm.Main.init({
          flags: { formFields: { posts: [ "id", "title", "content" ]} }
      })

-}
withFormFields : Dict String (List String) -> Config m msg -> Config m msg
withFormFields =
    Config.withFormFields


{-| Mount an application on a give path using
[Url.Parser](https://package.elm-lang.org/packages/elm/url/latest/Url.Parser).
This is usefull if you want to override an existing page or add additional
behaviour.

The component specification is similar to the specification for
[Browser.element](https://package.elm-lang.org/packages/elm/browser/latest/Browser#element),
with the addition of `onLogin` param for which a msg should be provided to be
sent on successful login.

Note that the type signature changes from
`PostgRestAdmin.Program Nothing Nothing`.
`Model` and `Msg` are defined by your application.

The url parser should map to a Msg to be used to `update` your application when
navigating to this route built the parameters that the parser defines, you can
use
[Url.Parser.oneOf](https://package.elm-lang.org/packages/elm/url/latest/Url.Parser#oneOf)
to parse many routes.

    main : PostgRestAdmin.Program Model Msg
    main =
        Config.init
            |> Config.withMountPoint
                { view = view
                , update = update
                , init = init
                , onLogin = LoggedIn
                }
                (Parser.map MyPostLoadedMsg
                    (s "posts" </> Parser.string </> s "comments")
                )
            |> PostgRestAdmin.application

The `application` is initialized with a [Client](PostgRestAdmin-Client) you can
use to perform requests.

-}
withMountPoint :
    { init : Client -> ( model, AppCmd.Cmd msg )
    , view : model -> Html msg
    , update : msg -> model -> ( model, AppCmd.Cmd msg )
    , onLogin : Client -> msg
    }
    -> Parser (msg -> Element model msg) (Element model msg)
    -> Config model msg
    -> Config model msg
withMountPoint =
    Config.withMountPoint


type alias Element m msg =
    Cmd (Msg m msg)
