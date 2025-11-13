module PostgRestAdmin.Config exposing
    ( Config
    , init
    , host
    , mountPath
    , formFields
    , detailActions
    , menuLinks
    , tables
    , tableAliases
    , formAuth
    , jwt
    , onLogin
    , onAuthFailed
    , onExternalLogin
    , onLogout
    , routes
    , flagsDecoder
    )

{-|


# Program configuration

@docs Config


# Init

@docs init


# Basics

@docs host
@docs mountPath
@docs formFields
@docs detailActions
@docs menuLinks


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
@docs flagsDecoder

-}

import Browser.Navigation as Nav
import Dict exposing (Dict)
import Html exposing (Html)
import Internal.Cmd as AppCmd
import Internal.Config as Config
import Internal.Schema exposing (Record)
import Json.Decode as Decode
import PostgRestAdmin.Client exposing (Client)
import PostgRestAdmin.Config.FormAuth exposing (FormAuth)
import PostgRestAdmin.MountPath exposing (MountPath)
import Url.Parser exposing (Parser)


{-| [PostgRestAdmin.application](PostgRestAdmin#application) configuration
params.
-}
type alias Config f m msg =
    Decode.Decoder (Config.Config f m msg)


{-| [PostgRestAdmin.application](PostgRestAdmin#application) decoder with
defaults.

    main : PostgRestAdmin.Program Never Never Never
    main =
        PostgRestAdmin.application Config.init

-}
init : Config f m msg
init =
    Config.init


{-| Specify the postgREST host.

    main : PostgRestAdmin.Program Never Never Never
    main =
        Config.init
            |> Config.host "http://localhost:3000"
            |> PostgRestAdmin.application

Alternatively the host can be specified using flags, configuring using `host`.
Program flags take precedence.

    Elm.Main.init({ "flags" : { "host" : "http://localhost:3000" }})

-}
host : String -> Config f m msg -> Config f m msg
host =
    Config.host


{-| Specify a path prefix for all routes, in case the app is not mounted in the
root path.

    main : PostgRestAdmin.Program Never Never Never
    main =
        Config.init
            |> Config.mountPath "/back-office"
            |> PostgRestAdmin.application

Alternatively the host can be specified using flags, configuring using
`mountPath`. Program flags take precedence.

    Elm.Main.init({ "flags" : { "mountPath" : "/back-office" }})

-}
mountPath : String -> Config f m msg -> Config f m msg
mountPath =
    Config.mountPath


{-| Enable user credentials form and configure the parameters. Credentials
are be used to obtain a JWT.

See [FormAuth](PostgRestAdmin.FormAuth) for configuration options.

    import PostgRestAdmin.Config.FormAuth as FormAuth

    main : PostgRestAdmin.Program Never Never Never
    main =
        Config.init
            |> Config.formAuth FormAuth.config
            |> PostgRestAdmin.application

-}
formAuth : FormAuth -> Config f m msg -> Config f m msg
formAuth =
    Config.formAuth


{-| Set a JWT to authenticate postgREST requests. Even when using
[formAuth](#formAuth) it's possible to set an initial JWT.

    main : PostgRestAdmin.Program Never Never Never
    main =
        Config.init
            |> Config.jwt "8abf3a...9ac36d"
            |> PostgRestAdmin.application

Alternatively the token can be passed using flags, configuring using `jwt`.
Program flags take precedence.

    Elm.Main.init({
        "flags" : { "jwt" : sessionStorage.getItem("jwt") }
     })

-}
jwt : String -> Config f m msg -> Config f m msg
jwt =
    Config.jwt


{-| Callback triggered with a JWT string on successful login.
Tipically used to persist the JWT to session storage.

    port loginSuccess : String -> Cmd msg

    main : PostgRestAdmin.Program Never Never Never
    main =
        Config.init
            |> Config.onLogin loginSuccess
            |> PostgRestAdmin.application

Then subscribe to the corresponding port.

    app = Elm.Main.init({
      flags: { jwt: sessionStorage.getItem("jwt") }
    })

    app.ports.loginSuccess.subscribe(jwt => {
      sessionStorage.setItem("jwt", jwt)
    });

-}
onLogin : (String -> Cmd msg) -> Config f m msg -> Config f m msg
onLogin =
    Config.onLogin


{-| Callback triggered when authentication fails when attempting to perform a
request. You can use to perform external authentication.

    port authFailure : String -> Cmd msg

    port tokenReceiver :
        ({ path : String, accessToken : String } -> msg)
        -> Sub msg

    main : PostgRestAdmin.Program Never Never Never
    main =
        Config.init
            |> Config.withAuthFailed authFailed
            |> Config.onExternalLogin tokenReceiver
            |> PostgRestAdmin.application

Then wire to the corresponding ports.

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
onAuthFailed : (String -> Cmd msg) -> Config f m msg -> Config f m msg
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
    -> Config f m msg
    -> Config f m msg
onExternalLogin =
    Config.onExternalLogin


{-| Callback triggered when authentication fails when attempting to perform a
request. You can use to perform external authentication.

    port logout : () -> Cmd msg

    main : PostgRestAdmin.Program Never Never Never
    main =
        Config.init
            |> Config.onLogout logout
            |> PostgRestAdmin.application

Then subscribe to the corresponding port.

    app = Elm.Main.init()

    app.ports.logout.subscribe(_ => {
        externalLogout()
    });

-}
onLogout : (() -> Cmd msg) -> Config f m msg -> Config f m msg
onLogout =
    Config.onLogout


{-| Specify which fields should be present in the the edit and create forms,
overriding the table schema. By default a primary key field is not present in
the forms.

    main : PostgRestAdmin.Program Never Never Never
    main =
        Config.init
            |> Config.formFields "posts" [ "id", "title", "content" ]
            |> PostgRestAdmin.application

Alternatively this parameter can be configured passing the flag `formFields`.
Program flags take precedence.

    Elm.Main.init({
        "flags" : { "formFields" : { "posts" : [ "id", "title", "content" ] } }
    })

-}
formFields : String -> List String -> Config f m msg -> Config f m msg
formFields =
    Config.formFields


{-| Specify a number of actions buttons to be shown in the detail page of a
record along with Edit and Delete buttons.

`detailActions` expect a dict where the keys correspond with the name of a
table and the values are a list of tuples, the first element of the tuple
corresponds to the button text and the second is a function that takes the id of
the resource and returns a url string.

    import Url.Builder as Url

    main : PostgRestAdmin.Program Never Never Never
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
    -> Config f m msg
    -> Config f m msg
detailActions =
    Config.detailActions


{-| Pass a list of table names to restrict the editable resources, also sets the
order of the left resources menu.

    main : PostgRestAdmin.Program Never Never Never
    main =
        Config.init
            |> Config.tables [ "posts", "comments" ]
            |> PostgRestAdmin.application

Alternatively the host can be specified passing the flag `tables`.
Program flags take precedence.

    Elm.Main.init({ "tables" : [ "posts", "comments" ]})

-}
tables : List String -> Config f m msg -> Config f m msg
tables =
    Config.tables


{-| Pass a dict of links to display in the side menu. The list consists of a
tuples of the link text and a url.

    main : PostgRestAdmin.Program Never Never Never
    main =
        Config.init
            |> Config.menuLinks [ ( "Api Docs", "/api/docs" ) ]
            |> PostgRestAdmin.application

Alternatively the menu links can be specified passing the flag `menuLinks`.
Program flags take precedence.

    Elm.Main.init({
        "menuLinks" : [
             { text : "Api Docs", url : "/api/docs" }
        ]
    })

-}
menuLinks : List ( String, String ) -> Config f m msg -> Config f m msg
menuLinks =
    Config.menuLinks


{-| Rename a table referenced in a foreign key. PostgREST OpenApi genreated docs
confuses tables with views when describing the foreign key for a resource,
because of this some links might be incorrectly generated.

    main : PostgRestAdmin.Program Never Never Never
    main =
        Config.init
            |> Config.tableAliases
                (Dict.fromList [ ( "published_posts", "posts" ) ])
            |> PostgRestAdmin.application

Alternatively the host can be specified using flags, configuring using
`tableAliases`.
Program flags take precedence.

    Elm.Main.init({
        tableAliases: { "published_posts" : "posts "}
    })

-}
tableAliases : Dict String String -> Config f m msg -> Config f m msg
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
`PostgRestAdmin.Program Never Nothing Nothing`.
`Model` and `Msg` are defined by your application.

The url parser should map to a Msg to be used to `update` your application when
navigating to this route built the parameters that the parser defines, you can
use
[Url.Parser.oneOf](https://package.elm-lang.org/packages/elm/url/latest/Url.Parser#oneOf)
to parse many routes.

    main : PostgRestAdmin.Program Never Model Msg
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
    { init :
        { flags : flags
        , client : Client
        , mountPath : MountPath
        }
        -> Nav.Key
        -> ( model, AppCmd.Cmd msg )
    , view : model -> Html msg
    , update : msg -> model -> ( model, AppCmd.Cmd msg )
    , subscriptions : model -> Sub msg
    , onLogin : Client -> msg
    }
    -> Parser (msg -> msg) msg
    -> Config flags model msg
    -> Config flags model msg
routes =
    Config.routes


{-| Decode flags to be passed to the `init` function for an application mounted
using [routes](#routes).

    main : PostgRestAdmin.Program String Model Msg
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
            |> Config.flagsDecoder (Decode.field "hostUrl" Decode.string)
            |> PostgRestAdmin.application

The `application` is initialized with a [Client](PostgRestAdmin-Client) you can
use to perform requests.

-}
flagsDecoder :
    Decode.Decoder flags
    -> Config flags model msg
    -> Config flags model msg
flagsDecoder =
    Config.flagsDecoder
