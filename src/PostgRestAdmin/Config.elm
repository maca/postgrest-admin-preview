module PostgRestAdmin.Config exposing
    ( Config
    , init
    , host
    , mountPoint
    , formFields
    , detailActions
    , tables
    , tableAliases
    , formAuth
    , jwt
    , onLogin
    , onAuthFailed
    , onExternalLogin
    , onLogout
    , routes
    )

{-| Program configuration

@docs Config


# Init

@docs init


# Basics

@docs host
@docs mountPoint
@docs formFields
@docs detailActions


# Tables

@docs tables
@docs tableAliases


# Auth

@docs formAuth
@docs jwt
@docs onLogin
@docs onAuthFailed
@docs onExternalLogin
@docs onLogout


# Application mounting

@docs routes

-}

import Browser.Navigation as Nav
import Dict exposing (Dict)
import Html exposing (Html)
import Internal.Cmd as AppCmd
import Internal.Config as Config
import Json.Decode exposing (Decoder)
import PostgRestAdmin.Client exposing (Client)
import PostgRestAdmin.Config.FormAuth exposing (FormAuth)
import PostgRestAdmin.Record exposing (Record)
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
              |> Config.host "http://localhost:3000"
              |> PostgRestAdmin.application

Alternatively the host can be specified using flags, configuring using `host`.
Program flags take precedence.

      Elm.Main.init({
          flags: { host: "http://localhost:3000" }
      })

-}
host : String -> Config m msg -> Config m msg
host =
    Config.host


{-| Specify a path prefix for all routes, in case the app is not mounted in the
root path.

      main : PostgRestAdmin.Program Never Never
      main =
          Config.init
              |> Config.mountPoint "/back-office"
              |> PostgRestAdmin.application

Alternatively the host can be specified using flags, configuring using
`mountPoint`. Program flags take precedence.

      Elm.Main.init({
          flags: { mountPoint: "/back-office" }
      })

-}
mountPoint : String -> Config m msg -> Config m msg
mountPoint =
    Config.mountPoint


{-| Enable user credentials form and configure the parameters. Credentials
are be used to obtain a JWT.

See [FormAuth](PostgRestAdmin.FormAuth) for configuration options.

      import PostgRestAdmin.Config.FormAuth as FormAuth

      main : PostgRestAdmin.Program Never Never
      main =
          Config.init
              |> Config.formAuth FormAuth.config
              |> PostgRestAdmin.application

-}
formAuth : FormAuth -> Config m msg -> Config m msg
formAuth =
    Config.formAuth


{-| Set a JWT to authenticate postgREST requests. Even when using
[formAuth](#formAuth) it's possible to set an initial JWT.

      main : PostgRestAdmin.Program Never Never
      main =
          Config.init
              |> Config.jwt "8abf3a...9ac36d"
              |> PostgRestAdmin.application

Alternatively the token can be passed using flags, configuring using `jwt`.
Program flags take precedence.

      Elm.Main.init({
          flags: { jwt: sessionStorage.getItem("jwt") }
      })

-}
jwt : String -> Config m msg -> Config m msg
jwt =
    Config.jwt


{-| Callback triggered with a JWT string on successful login.
Tipically used to persist the JWT to session storage.

          port loginSuccess : String -> Cmd msg

          main : PostgRestAdmin.Program Never Never
          main =
              Config.init
                  |> Config.onLogin loginSuccess
                  |> PostgRestAdmin.application

// Elm init

        app = Elm.Main.init({
          flags: { jwt: sessionStorage.getItem("jwt") }
        })

        app.ports.loginSuccess.subscribe(jwt => {
          sessionStorage.setItem("jwt", jwt)
        });

-}
onLogin : (String -> Cmd msg) -> Config m msg -> Config m msg
onLogin =
    Config.onLogin


{-| Callback triggered when authentication fails when attempting to perform a
request. You can use to perform external authentication.

          port authFailure : String -> Cmd msg

          port tokenReceiver :
              ({ path : String, accessToken : String } -> msg)
              -> Sub msg


          main : PostgRestAdmin.Program Never Never
          main =
              Config.init
                  |> Config.withAuthFailed authFailed
                  |> Config.onExternalLogin tokenReceiver
                  |> PostgRestAdmin.application

// Elm init

        app = Elm.Main.init()

        app.ports.authFailure.subscribe(requestedPath => {
            authenticate(requestedPath).then((accessToken) => {
                app.ports.tokenReceiver.send({
                    path : requestedPath,
                    accessToken : accessToken
                })

            })
        });

-}
onAuthFailed : (String -> Cmd msg) -> Config m msg -> Config m msg
onAuthFailed =
    Config.onAuthFailed


{-| Subscribe to receive a JWT and a redirect path when login with an external
provider.

See [onAuthFailed](#onAuthFailed).

-}
onExternalLogin :
    (({ path : String, accessToken : String }
      -> { path : String, accessToken : String }
     )
     -> Sub { path : String, accessToken : String }
    )
    -> Config m msg
    -> Config m msg
onExternalLogin =
    Config.onExternalLogin


{-| Callback triggered when authentication fails when attempting to perform a
request. You can use to perform external authentication.

          port logout : () -> Cmd msg

          main : PostgRestAdmin.Program Never Never
          main =
              Config.init
                  |> Config.onLogout logout
                  |> PostgRestAdmin.application

// Elm init

        app = Elm.Main.init()

        app.ports.logout.subscribe(_ => {
            externalLogout()
        });

-}
onLogout : (() -> Cmd msg) -> Config m msg -> Config m msg
onLogout =
    Config.onLogout


{-| Specify which fields should be present in the the edit and create forms,
overriding the table schema. By default a primary key field is not present in
the forms.

      main : PostgRestAdmin.Program Never Never
      main =
          Config.init
              |> Config.formFields "posts" ["id", "title", "content"]
              |> PostgRestAdmin.application

Alternatively this parameter can be configured using flags, configuring using
`formFields`. Program flags take precedence.

      Elm.Main.init({
          flags: { formFields: { posts: [ "id", "title", "content" ]} }
      })

-}
formFields : String -> List String -> Config m msg -> Config m msg
formFields =
    Config.formFields


{-| Specify a number of actions buttons to be shown in the detail page of a
record along with Edit and Delete buttons.

`detailActions` expect a dict where the keys correspond with the name of a
table and the values are a list of tuples, the first element of the tuple
corresponds to the button text and the second is a function that takes the id of
the resource and returns a url string.

      import Url.Builder as Url

      main : PostgRestAdmin.Program Never Never
      main =
          Config.init
              |> Config.detailActions "posts"
                  [ ( "View Comments"
                    , \_ id -> Url.absolute [ "posts", id, "comments" ] []
                    )
                  ]

          |> PostgRestAdmin.application

-}
detailActions :
    String
    -> List ( String, Record -> String -> String )
    -> Config m msg
    -> Config m msg
detailActions =
    Config.detailActions


{-| Pass a list of table names to restrict the editable resources, also sets the
order of the left resources menu.

      main : PostgRestAdmin.Program Never Never
      main =
          Config.init
              |> Config.tables ["posts", "comments"]
              |> PostgRestAdmin.application

Alternatively the host can be specified using flags, configuring using
`tables`.
Program flags take precedence.

      Elm.Main.init({
          tables: ["posts", "comments"]
      })

-}
tables : List String -> Config m msg -> Config m msg
tables =
    Config.tables


{-| Rename a table referenced in a foreign key. PostgREST OpenApi genreated docs
confuses tables with views when describing the foreign key for a resource,
because of this some links might be incorrectly generated.

      main : PostgRestAdmin.Program Never Never
      main =
          Config.init
              |> Config.tableAliases
                   (Dict.fromList [("published_posts", "posts")])
              |> PostgRestAdmin.application

Alternatively the host can be specified using flags, configuring using
`tableAliases`.
Program flags take precedence.

      Elm.Main.init({
          tableAliases: { "published_posts" : "posts "}
      })

-}
tableAliases : Dict String String -> Config m msg -> Config m msg
tableAliases =
    Config.tableAliases


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
            |> Config.routes
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
routes :
    { init : Client -> Nav.Key -> ( model, AppCmd.Cmd msg )
    , view : model -> Html msg
    , update : msg -> model -> ( model, AppCmd.Cmd msg )
    , subscriptions : model -> Sub msg
    , onLogin : Client -> msg
    }
    -> Parser (msg -> msg) msg
    -> Config model msg
    -> Config model msg
routes =
    Config.routes
