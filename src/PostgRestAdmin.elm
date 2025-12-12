module PostgRestAdmin exposing
    ( Config(..), Program, configure, buildProgram, buildAppParams
    , onLogin, onAuthFailed, onLogout, onExternalLogin
    , withHost, withLoginUrl, withJwt, withClientHeaders
    , withMountPath, withRecordsPerPage
    , withMenuLinks, withFormFields, withDetailActions
    , withTables, withTableAliases
    , withLoginBannerText
    , Params, configDecoder
    )

{-|

@docs Config, Program, configure, buildProgram, buildAppParams


## Wiring

@docs onLogin, onAuthFailed, onLogout, onExternalLogin


## Client

@docs withHost, withLoginUrl, withJwt, withClientHeaders
@docs withFlags


## UI

@docs withMountPath, withRecordsPerPage
@docs withMenuLinks, withFormFields, withDetailActions
@docs withTables, withTableAliases
@docs withLoginBannerText

-}

import Browser
import Browser.Navigation as Nav
import Dict exposing (Dict)
import FormToolkit.Field as Field
import FormToolkit.Parse as Parse
import Html exposing (Html)
import Html.Attributes as Attrs
import Html.Events as Events
import Http
import Internal.Cmd as AppCmd exposing (AppCmd)
import Internal.Flag as Flag
import Internal.PageDetail as PageDetail
import Internal.PageForm as PageForm
import Internal.PageListing as PageListing exposing (Model)
import Internal.Schema exposing (Record, Schema)
import Json.Decode as Decode exposing (Decoder)
import Json.Encode as Encode
import Markdown
import PostgRestAdmin.Client as Client exposing (AuthScheme, Client, Error(..))
import PostgRestAdmin.MountPath as MountPath exposing (MountPath, path)
import Postgrest.Client as PG
import Process
import String.Extra as String
import Task
import Url exposing (Protocol(..), Url)
import Url.Parser as Parser exposing ((</>), Parser, s)


{-| An alias to elm's Platform.Program providing the type signature for a
PostgRestAdmin program.
-}
type alias Program =
    Platform.Program Decode.Value Model Msg


type alias Model =
    { route : Route
    , key : Nav.Key
    , notification : Notification
    , error : Maybe String
    , client : Client
    , currentUrl : Url
    , authFormUrl : Url
    , authFormJwtDecoder : Decoder String
    , authFormJwtEncoder : Dict String String -> Encode.Value
    , authFormField : Field.Field Never
    , authFormStatus : AuthFormStatus
    , onLogin : String -> Cmd Msg
    }


type alias Params msg =
    { host : Url
    , mountPath : MountPath
    , loginUrl : Url
    , authScheme : AuthScheme
    , formFields : Dict String (List String)
    , detailActions : Dict String (List ( String, Record -> String -> String ))
    , tables : List String
    , menuLinks : List ( String, String )
    , menuActions : Dict String Url
    , tableAliases : Dict String String
    , clientHeaders : List Http.Header
    , recordsPerPage : Int
    , loginBannerText : Maybe String
    , onLogin : String -> Cmd msg
    , onAuthFailed : String -> Cmd msg
    , onExternalLogin :
        ({ path : String, accessToken : String }
         -> { path : String, accessToken : String }
        )
        -> Sub { path : String, accessToken : String }
    , onLogout : () -> Cmd msg
    }


type AuthFormStatus
    = Ready
    | Active
    | Submitting
    | Failure Client.Error


type Config msg
    = Config (Params msg)


type Msg
    = AuthFieldsChanged (Field.Msg Never)
    | AuthFormSubmitted
    | GotToken (Result Client.Error String)
    | SchemaFetched (Result Client.Error Schema)
    | PageListingChanged PageListing.Msg
    | PageDetailChanged PageDetail.Msg
    | PageFormChanged PageForm.Msg
    | NotificationDismiss
    | NotificationConfirm String
    | NotificationAlert String
    | LinkClicked Browser.UrlRequest
    | UrlChanged Url
    | LoggedIn { path : String, accessToken : String }
    | LoggedOut
    | AuthRequired Error
    | NoOp


type Route
    = RouteRoot
    | RouteLoadingSchema Url
    | RouteListing PageListing.Model
    | RouteDetail PageDetail.Model
    | RouteForm PageForm.Model
    | RouteNotFound


type Notification
    = Confirmation String
    | Error String
    | NoNotification


{-| Creates a default configuration.

    main : PostgRestAdmin.Program
    main =
        PostgRestAdmin.configure
            |> PostgRestAdmin.host "http://localhost:3000"
            |> PostgRestAdmin.onLogin loginSuccess
            |> PostgRestAdmin.buildProgram

-}
configure : Config msg
configure =
    Config defaultConfig


{-| Converts a Config into a Program.
-}
buildProgram : Config msg -> Program
buildProgram config =
    let
        params =
            buildAppParams
                { toInnerModel = identity
                , toOuterModel = identity
                , toInnerMsg = Just
                , toOuterMsg = identity
                }
                config
    in
    Browser.application
        { init = \_ -> params.init
        , update = params.update
        , view = \model -> { title = "Admin", body = [ params.view model ] }
        , subscriptions = params.subscriptions
        , onUrlRequest = params.onUrlRequest
        , onUrlChange = params.onUrlChange
        }


buildAppParams :
    { toInnerModel : model -> Model
    , toOuterModel : Model -> model
    , toInnerMsg : outerMsg -> Maybe Msg
    , toOuterMsg : Msg -> outerMsg
    }
    -> Config msg
    ->
        { init : Url -> Nav.Key -> ( model, Cmd outerMsg )
        , view : model -> Html outerMsg
        , update : outerMsg -> model -> ( model, Cmd outerMsg )
        , subscriptions : model -> Sub outerMsg
        , onUrlRequest : Browser.UrlRequest -> outerMsg
        , onUrlChange : Url -> outerMsg
        }
buildAppParams mappings (Config config) =
    { init =
        \url key ->
            let
                model =
                    initModel url key config

                ( route, cmd ) =
                    parseRoute config model.client url key
            in
            ( mappings.toOuterModel { model | route = route }
            , Cmd.map mappings.toOuterMsg cmd
            )
    , update =
        \outerMsg outerModel ->
            case mappings.toInnerMsg outerMsg of
                Nothing ->
                    ( outerModel, Cmd.none )

                Just msg ->
                    update config msg (mappings.toInnerModel outerModel)
                        |> Tuple.mapFirst mappings.toOuterModel
                        |> Tuple.mapSecond
                            (Cmd.map mappings.toOuterMsg)
    , view =
        mappings.toInnerModel
            >> view config
            >> Html.div []
            >> Html.map mappings.toOuterMsg
    , subscriptions =
        mappings.toInnerModel
            >> subscriptions config
            >> Sub.map mappings.toOuterMsg
    , onUrlRequest = mappings.toOuterMsg << LinkClicked
    , onUrlChange = mappings.toOuterMsg << UrlChanged
    }



-- INIT


initModel : Url -> Nav.Key -> Params msg -> Model
initModel url key configRec =
    { route = RouteRoot
    , key = key
    , notification = NoNotification
    , error = Nothing
    , client = Client.init configRec.host configRec.authScheme configRec.clientHeaders
    , onLogin = configRec.onLogin >> Cmd.map (always NoOp)
    , currentUrl = url
    , authFormUrl = configRec.loginUrl
    , authFormJwtDecoder = Decode.field "token" Decode.string
    , authFormJwtEncoder = Encode.dict identity Encode.string
    , authFormField = authFormField
    , authFormStatus = Ready
    }


authFormField : Field.Field Never
authFormField =
    Field.group []
        [ Field.text
            [ Field.name "email"
            , Field.label "Login"
            , Field.required True
            ]
        , Field.password
            [ Field.name "password"
            , Field.label "Password"
            , Field.required True
            ]
        ]


requestToken : Model -> Cmd Msg
requestToken model =
    Task.attempt GotToken
        (Http.task
            { method = "POST"
            , headers = []
            , url = Url.toString model.authFormUrl
            , body =
                Http.jsonBody
                    (Parse.parse Parse.json model.authFormField
                        |> Result.withDefault Encode.null
                    )
            , resolver = Client.jsonResolver model.authFormJwtDecoder
            , timeout = Nothing
            }
        )



-- UPDATE


update : Params msg -> Msg -> Model -> ( Model, Cmd Msg )
update config msg model =
    let
        client =
            model.client
    in
    case msg of
        AuthFieldsChanged toInnerMsg ->
            ( { model
                | authFormField = Field.update toInnerMsg model.authFormField
                , authFormStatus = Active
              }
            , Cmd.none
            )

        AuthFormSubmitted ->
            ( { model | authFormStatus = Submitting }
            , requestToken model
            )

        GotToken (Ok tokenStr) ->
            let
                updatedClient =
                    { client | authScheme = Client.Jwt (PG.jwt tokenStr) }
            in
            ( { model
                | client = updatedClient
                , authFormField = authFormField
              }
            , Cmd.batch
                [ model.onLogin tokenStr
                , Client.fetchSchema config updatedClient
                    |> Task.attempt SchemaFetched
                ]
            )

        GotToken (Err error) ->
            ( { model | authFormStatus = Failure error }
            , Cmd.none
            )

        SchemaFetched (Ok schema) ->
            let
                updatedClient =
                    { client | schema = schema }

                ( route, cmd ) =
                    routeCons config client model.currentUrl model.key
            in
            ( { model | client = updatedClient, route = route }
            , cmd
            )

        SchemaFetched (Err err) ->
            ( { model
                | client = Client.logout model.client
                , authFormStatus = Failure err
              }
            , Cmd.none
            )

        PageListingChanged childMsg ->
            case model.route of
                RouteListing listing ->
                    let
                        ( route, appCmd ) =
                            PageListing.update childMsg listing
                                |> Tuple.mapFirst RouteListing
                    in
                    ( { model | route = route }
                    , mapCmd PageListingChanged appCmd
                    )

                _ ->
                    ( model, Cmd.none )

        PageDetailChanged childMsg ->
            case model.route of
                RouteDetail prevDetail ->
                    let
                        ( detail, cmd ) =
                            PageDetail.update childMsg prevDetail
                    in
                    ( { model | route = RouteDetail detail }
                    , mapCmd PageDetailChanged cmd
                    )

                _ ->
                    ( model, Cmd.none )

        PageFormChanged childMsg ->
            case model.route of
                RouteForm prevForm ->
                    let
                        ( form, cmd ) =
                            PageForm.update childMsg prevForm
                    in
                    ( { model | route = RouteForm form }
                    , mapCmd PageFormChanged cmd
                    )

                _ ->
                    ( model, Cmd.none )

        NotificationDismiss ->
            ( { model | notification = NoNotification }
            , Cmd.none
            )

        NotificationConfirm message ->
            ( { model | notification = Confirmation message }
            , Process.sleep 5000
                |> Task.perform (always NotificationDismiss)
            )

        NotificationAlert message ->
            ( { model | notification = Error message }
            , Cmd.none
            )

        LinkClicked (Browser.Internal url) ->
            ( model
            , Cmd.batch
                [ Nav.pushUrl model.key (Url.toString url)
                , Task.succeed NotificationDismiss
                    |> Task.perform identity
                ]
            )

        LinkClicked (Browser.External href) ->
            ( model, Nav.load href )

        UrlChanged url ->
            if model.currentUrl.path /= url.path then
                let
                    ( route, cmd ) =
                        parseRoute config model.client url model.key
                in
                ( { model | route = route, currentUrl = url }, cmd )

            else
                ( { model | currentUrl = url }, Cmd.none )

        LoggedIn updateParams ->
            ( { model | client = Client.updateJwt updateParams.accessToken model.client }
            , Nav.pushUrl model.key updateParams.path
            )

        LoggedOut ->
            ( { model | client = Client.logout model.client }
            , Cmd.map (always NoOp) (config.onLogout ())
            )

        AuthRequired err ->
            ( { model
                | client = Client.logout model.client
                , authFormStatus = Failure err
                , notification = NoNotification
              }
            , Cmd.map (always NoOp)
                (config.onAuthFailed (urlToPath model.currentUrl))
            )

        NoOp ->
            ( model, Cmd.none )



-- VIEW


view : Params msg -> Model -> List (Html Msg)
view config model =
    case model.error of
        Just error ->
            [ Html.h1 [] [ Html.text "Init failed" ]
            , Html.pre
                [ Attrs.class "parse-errors" ]
                [ Html.text error ]
            ]

        Nothing ->
            case model.client.authScheme of
                Client.Unset ->
                    [ viewAuthForm config model ]

                _ ->
                    [ Html.div
                        []
                        [ Html.div
                            [ Attrs.class "main-container" ]
                            [ sideMenu config model
                            , Html.div
                                [ Attrs.class "main-area" ]
                                [ viewNotification model.notification
                                , mainContent model
                                ]
                            ]
                        ]
                    ]


viewAuthForm : Params msg -> Model -> Html Msg
viewAuthForm config model =
    Html.div
        [ Attrs.class "auth-modal overlay" ]
        [ Html.div
            [ Attrs.class "auth-form" ]
            [ errorMessage model.authFormStatus
            , Html.form
                [ Attrs.class "auth-form"
                , Events.onSubmit AuthFormSubmitted
                ]
                [ Field.toHtml AuthFieldsChanged model.authFormField
                , Html.button
                    [ Attrs.disabled
                        (Parse.parse Parse.json model.authFormField
                            |> Result.map (\_ -> False)
                            |> Result.withDefault True
                        )
                    ]
                    [ Html.text "Login" ]
                ]
            , case config.loginBannerText of
                Just text ->
                    Markdown.toHtml [ Attrs.class "login-banner" ] text

                Nothing ->
                    Html.text ""
            ]
        ]


errorMessage : AuthFormStatus -> Html Msg
errorMessage status =
    case status of
        Failure error ->
            Html.div
                [ Attrs.class "form-error-message" ]
                (case error of
                    Client.Forbidden ->
                        [ Html.text """You may have entered the wrong password, please try again.""" ]

                    Client.Unauthorized ->
                        [ Html.text "Please sign in to continue." ]

                    Client.BadStatus statusCode err ->
                        [ Html.text "The server responded with an error: "
                        , Html.pre []
                            [ Html.text
                                (err |> Maybe.withDefault (String.fromInt statusCode))
                            ]
                        ]

                    Client.NetworkError ->
                        [ Html.text """There was an issue reaching the server, please try again later.""" ]

                    _ ->
                        [ Html.text (Client.errorToString error) ]
                )

        _ ->
            Html.div
                [ Attrs.class "form-error-message"
                , Attrs.style "visibility" "hidden"
                ]
                []


sideMenu : Params msg -> Model -> Html Msg
sideMenu config model =
    Html.div
        [ Attrs.class "side-menu" ]
        [ Html.aside
            [ Attrs.class "resources-menu" ]
            [ Html.ul [] (List.map (menuItem config.mountPath) (resources config model.client))
            , Html.ul [] (List.map extraMenuItem config.menuLinks)
            ]
        , Html.div
            [ Attrs.class "account-management" ]
            [ Html.button
                [ Events.onClick LoggedOut
                , Attrs.class "button button-clear"
                ]
                [ Html.i [ Attrs.class "gg-log-out" ] []
                , Html.text "Logout"
                ]
            ]
        ]


menuItem : MountPath -> String -> Html Msg
menuItem mount name =
    Html.li
        []
        [ Html.a
            [ Attrs.href (path mount name) ]
            [ Html.text (String.toTitleCase (String.humanize name)) ]
        ]


extraMenuItem : ( String, String ) -> Html Msg
extraMenuItem ( linkText, url ) =
    Html.li
        []
        [ Html.a
            [ Attrs.href url ]
            [ Html.text (String.toTitleCase linkText) ]
        ]


viewNotification : Notification -> Html Msg
viewNotification notification =
    case notification of
        Confirmation text ->
            viewNotificationHelp "confirmation" text

        Error text ->
            viewNotificationHelp "error" text

        NoNotification ->
            Html.text ""


viewNotificationHelp : String -> String -> Html Msg
viewNotificationHelp notificationType message =
    Html.div
        [ Attrs.class "notification"
        , Attrs.class notificationType
        ]
        [ Html.pre [] [ Html.text message ]
        , Html.div [ Attrs.class "close" ]
            [ Html.i
                [ Attrs.class "icono-cross"
                , Events.onClick NotificationDismiss
                ]
                []
            ]
        ]


mainContent : Model -> Html Msg
mainContent model =
    case model.route of
        RouteRoot ->
            Html.text ""

        RouteLoadingSchema _ ->
            Html.text "Not found"

        RouteListing listing ->
            Html.map PageListingChanged (PageListing.view listing)

        RouteDetail listing ->
            Html.map PageDetailChanged (PageDetail.view listing)

        RouteForm form ->
            Html.map PageFormChanged (PageForm.view form)

        RouteNotFound ->
            Html.text "Not found"



-- SUBSCRIPTIONS


subscriptions : Params msg -> Model -> Sub Msg
subscriptions config model =
    Sub.batch
        [ case model.route of
            RouteListing pageListing ->
                Sub.map PageListingChanged
                    (PageListing.subscriptions pageListing)

            _ ->
                Sub.none
        , Sub.map LoggedIn (config.onExternalLogin identity)
        ]



-- ROUTES


parseRoute : Params msg -> Client -> Url -> Nav.Key -> ( Route, Cmd Msg )
parseRoute config client url key =
    if Client.schemaIsLoaded client then
        routeCons config client url key

    else
        ( RouteLoadingSchema url
        , Task.attempt SchemaFetched
            (Client.fetchSchema config client)
        )


routeCons :
    Params msg
    -> Client
    -> Url
    -> Nav.Key
    -> ( Route, Cmd Msg )
routeCons config client url nav =
    Parser.parse
        (List.foldr (\p acc -> s p </> acc)
            (routeParser config client url nav)
            (MountPath.segments config.mountPath)
        )
        url
        |> Maybe.withDefault ( RouteNotFound, Cmd.none )


routeParser :
    Params msg
    -> Client
    -> Url
    -> Nav.Key
    -> Parser (( Route, Cmd Msg ) -> a) a
routeParser config client url key =
    let
        params =
            { client = client
            , key = key
            , config = config
            }
    in
    Parser.oneOf
        [ -- /
          Parser.map
            ( RouteRoot
            , resources config client
                |> List.head
                |> Maybe.map
                    (\p ->
                        Nav.pushUrl key
                            (MountPath.path config.mountPath p)
                    )
                |> Maybe.withDefault Cmd.none
            )
            Parser.top

        -- /posts/new
        , Parser.map
            (\tableName ->
                initForm
                    { client = params.client
                    , key = params.key
                    , config = params.config
                    , parent = Nothing
                    , tableName = tableName
                    , id = Nothing
                    }
            )
            (Parser.string </> s "new")

        -- /posts/edit
        , Parser.map
            (\tableName id ->
                initForm
                    { client = params.client
                    , key = params.key
                    , config = params.config
                    , parent = Nothing
                    , tableName = tableName
                    , id = Just id
                    }
            )
            (Parser.string </> Parser.string </> s "edit")

        -- /posts
        , Parser.map
            (\tableName ->
                initListing
                    { client = params.client
                    , key = params.key
                    , config = params.config
                    , url = url
                    , parent = Nothing
                    , tableName = tableName
                    }
            )
            Parser.string

        -- /posts/1
        , Parser.map
            (\tableName id ->
                initDetail
                    { client = params.client
                    , key = params.key
                    , config = params.config
                    , tableName = tableName
                    , id = id
                    }
            )
            (Parser.string </> Parser.string)

        -- /posts/1/comments
        , Parser.map
            (\parentTable parentId tableName ->
                initListing
                    { client = params.client
                    , key = params.key
                    , config = params.config
                    , url = url
                    , parent = Just { tableName = parentTable, id = parentId }
                    , tableName = tableName
                    }
            )
            (Parser.string </> Parser.string </> Parser.string)

        -- /posts/1/comments/new
        , Parser.map
            (\parentTable parentId tableName ->
                initForm
                    { client = params.client
                    , key = params.key
                    , config = params.config
                    , parent = Just { tableName = parentTable, id = parentId }
                    , tableName = tableName
                    , id = Nothing
                    }
            )
            (Parser.string </> Parser.string </> Parser.string </> s "new")
        ]


initListing :
    { client : Client
    , key : Nav.Key
    , config : Params msg
    , url : Url
    , parent : Maybe { tableName : String, id : String }
    , tableName : String
    }
    -> ( Route, Cmd Msg )
initListing params =
    case Dict.get params.tableName params.client.schema of
        Just table ->
            let
                listingParams =
                    { client = params.client
                    , mountPath = params.config.mountPath
                    , table = table
                    , parent = params.parent
                    , recordsPerPage = params.config.recordsPerPage
                    }
            in
            PageListing.init listingParams params.url params.key
                |> Tuple.mapFirst RouteListing
                |> Tuple.mapSecond (mapCmd PageListingChanged)

        Nothing ->
            ( RouteNotFound, Cmd.none )


initForm :
    { config : Params msg
    , client : Client
    , key : Nav.Key
    , parent : Maybe { tableName : String, id : String }
    , tableName : String
    , id : Maybe String
    }
    -> ( Route, Cmd Msg )
initForm params =
    case Dict.get params.tableName params.client.schema of
        Just table ->
            PageForm.init
                { client = params.client
                , navKey = params.key
                , mountPath = params.config.mountPath
                , parent = params.parent
                , table = table
                , id = params.id
                }
                |> Tuple.mapFirst RouteForm
                |> Tuple.mapSecond (mapCmd PageFormChanged)

        Nothing ->
            ( RouteNotFound, Cmd.none )


initDetail :
    { client : Client
    , key : Nav.Key
    , config : Params msg
    , tableName : String
    , id : String
    }
    -> ( Route, Cmd Msg )
initDetail params =
    case Dict.get params.tableName params.client.schema of
        Just table ->
            let
                detailParams =
                    { client = params.client
                    , mountPath = params.config.mountPath
                    , table = table
                    , id = params.id
                    , detailActions =
                        params.config.detailActions
                            |> Dict.get params.tableName
                            |> Maybe.withDefault []
                    }
            in
            PageDetail.init detailParams params.key
                |> Tuple.mapFirst RouteDetail
                |> Tuple.mapSecond (mapCmd PageDetailChanged)

        Nothing ->
            ( RouteNotFound, Cmd.none )



-- UTILS


resources : Params msg -> Client -> List String
resources config client =
    if List.isEmpty config.tables then
        Dict.keys client.schema |> List.sort

    else
        config.tables


mapCmd : (a -> Msg) -> AppCmd a -> Cmd Msg
mapCmd tagger appCmd =
    case appCmd of
        AppCmd.ChildCmd cmd ->
            Cmd.map tagger cmd

        AppCmd.Batch cmds ->
            Cmd.batch (List.map (mapCmd tagger) cmds)

        AppCmd.ClientError error ->
            Cmd.batch
                [ Task.succeed (Client.errorToString error)
                    |> Task.perform NotificationAlert
                , case error of
                    Unauthorized ->
                        Task.succeed (AuthRequired error)
                            |> Task.perform identity

                    _ ->
                        Cmd.none
                ]

        AppCmd.Error error ->
            Task.succeed error
                |> Task.perform NotificationAlert

        AppCmd.NotificationConfirm message ->
            Task.succeed message
                |> Task.perform NotificationConfirm

        AppCmd.NotificationDismiss ->
            Task.succeed NotificationDismiss
                |> Task.perform identity


urlToPath : Url -> String
urlToPath url =
    [ Just url.path, url.query ]
        |> List.filterMap identity
        |> String.join "?"



-- CONFIG


configDecoder : Params msg -> Decoder (Params msg)
configDecoder =
    Decode.succeed
        >> hostDecoder
        >> loginUrlDecoder
        >> jwtDecoder
        >> clientHeadersDecoder
        >> mountPathDecoder
        >> recordsPerPageDecoder
        >> menuLinksDecoder
        >> formFieldsDecoder
        >> tablesDecoder
        >> tableAliasesDecoder
        >> loginBannerTextDecoder


defaultConfig : Params msg
defaultConfig =
    let
        defaultHost =
            { protocol = Http
            , host = "localhost"
            , port_ = Just 3000
            , path = ""
            , query = Nothing
            , fragment = Nothing
            }
    in
    { authScheme = Client.unset
    , host = defaultHost
    , mountPath = MountPath.fromString ""
    , loginUrl = { defaultHost | path = "/rpc/login" }
    , formFields = Dict.empty
    , detailActions = Dict.empty
    , tables = []
    , menuLinks = []
    , menuActions = Dict.empty
    , onLogin = always Cmd.none
    , onAuthFailed = always Cmd.none
    , onExternalLogin = always Sub.none
    , onLogout = always Cmd.none
    , tableAliases = Dict.empty
    , clientHeaders = []
    , recordsPerPage = 50
    , loginBannerText = Nothing
    }


{-| Callback triggered with a JWT string on successful login.
Typically used to persist the JWT to session storage.

    port loginSuccess : String -> Cmd msg

    main : PostgRestAdmin.Program
    main =
        PostgRestAdmin.configuration
            |> PostgRestAdmin.onLogin loginSuccess
            |> PostgRestAdmin.buildProgram

Then subscribe to the corresponding port.

    app.ports.loginSuccess.subscribe(jwt => {
      sessionStorage.setItem("jwt", jwt)
    });

-}
onLogin : (String -> Cmd msg) -> Config msg -> Config msg
onLogin f (Config conf) =
    Config { conf | onLogin = f }


{-| Callback triggered when authentication fails when attempting to perform a
request. You can use this to perform external authentication.

    port authFailure : String -> Cmd msg

    port tokenReceiver :
        ({ path : String, accessToken : String } -> msg)
        -> Sub msg

    main : PostgRestAdmin.Program
    main =
        PostgRestAdmin.configuration
            |> PostgRestAdmin.onAuthFailed authFailure
            |> PostgRestAdmin.onExternalLogin tokenReceiver
            |> PostgRestAdmin.buildProgram

Then wire to the corresponding ports.

    app.ports.authFailure.subscribe(requestedPath => {
        authenticate(requestedPath).then((accessToken) => {
            app.ports.tokenReceiver.send({
                path : requestedPath,
                accessToken : accessToken
            })
        })
    });

-}
onAuthFailed : (String -> Cmd msg) -> Config msg -> Config msg
onAuthFailed f (Config conf) =
    Config { conf | onAuthFailed = f }


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
    -> Config msg
    -> Config msg
onExternalLogin sub (Config conf) =
    Config { conf | onExternalLogin = sub }


{-| Callback triggered when the user logs out.
You can use this to perform cleanup or external logout operations.

    port logout : () -> Cmd msg

    main : PostgRestAdmin.Program
    main =
        PostgRestAdmin.configuration
            |> PostgRestAdmin.onLogout logout
            |> PostgRestAdmin.buildProgram

Then subscribe to the corresponding port.

    app.ports.logout.subscribe(_ => {
        externalLogout()
    });

-}
onLogout : (() -> Cmd msg) -> Config msg -> Config msg
onLogout f (Config conf) =
    Config { conf | onLogout = f }


{-| Specify the PostgREST host.

    main : PostgRestAdmin.Program
    main =
        PostgRestAdmin.configuration
            |> PostgRestAdmin.withHost "http://localhost:3000"
            |> PostgRestAdmin.buildProgram

-}
withHost : String -> Config msg -> Config msg
withHost urlStr (Config conf) =
    case Url.fromString urlStr of
        Just u ->
            Config
                { conf
                    | host = u
                    , loginUrl = { u | path = "/rpc/login" }
                }

        Nothing ->
            Config conf


hostDecoder : Decoder (Params msg) -> Decoder (Params msg)
hostDecoder =
    Flag.string "host"
        (\urlStr conf ->
            Url.fromString urlStr
                |> Maybe.map
                    (\u ->
                        Decode.succeed
                            { conf
                                | host = u
                                , loginUrl = { u | path = "/rpc/login" }
                            }
                    )
                |> Maybe.withDefault
                    (Decode.fail "`Config.host` was given an invalid URL")
        )


{-| Specify the login URL for form authentication.

    main : PostgRestAdmin.Program
    main =
        PostgRestAdmin.configuration
            |> PostgRestAdmin.withLoginUrl "http://localhost:3000/rpc/login"
            |> PostgRestAdmin.buildProgram

-}
withLoginUrl : String -> Config msg -> Config msg
withLoginUrl urlStr (Config conf) =
    case Url.fromString urlStr of
        Just u ->
            Config { conf | loginUrl = u }

        Nothing ->
            Config conf


loginUrlDecoder : Decoder (Params msg) -> Decoder (Params msg)
loginUrlDecoder =
    Flag.string "loginUrl"
        (\urlStr conf ->
            Url.fromString urlStr
                |> Maybe.map (\u -> Decode.succeed { conf | loginUrl = u })
                |> Maybe.withDefault
                    (Decode.fail "`Config.loginUrl` was given an invalid URL")
        )


{-| Set a JWT to authenticate PostgREST requests. You can set an initial JWT
using this attribute.

    main : PostgRestAdmin.Program
    main =
        PostgRestAdmin.configuration
            |> PostgRestAdmin.withJwt "8abf3a...9ac36d"
            |> PostgRestAdmin.buildProgram

-}
withJwt : String -> Config msg -> Config msg
withJwt tokenStr (Config conf) =
    Config { conf | authScheme = Client.jwt tokenStr }


jwtDecoder : Decoder (Params msg) -> Decoder (Params msg)
jwtDecoder =
    Flag.string "jwt"
        (\tokenStr conf -> Decode.succeed { conf | authScheme = Client.jwt tokenStr })


{-| Set default HTTP headers to be included in all Client requests. This is
useful for setting headers like `Accept-Profile` or `Content-Profile` when
working with PostgREST schemas.

    main : PostgRestAdmin.Program
    main =
        PostgRestAdmin.configuration
            |> PostgRestAdmin.withClientHeaders
                [ Http.header "Accept-Profile" "bluebox"
                , Http.header "Content-Profile" "bluebox"
                ]
            |> PostgRestAdmin.buildProgram

-}
withClientHeaders : List Http.Header -> Config msg -> Config msg
withClientHeaders headers (Config conf) =
    Config { conf | clientHeaders = headers }


clientHeadersDecoder : Decoder (Params msg) -> Decoder (Params msg)
clientHeadersDecoder =
    Flag.headersList "clientHeaders"
        (\headers conf -> Decode.succeed { conf | clientHeaders = headers })


{-| Specify a path prefix for all routes, in case the app is not mounted in the
root path.

    main : PostgRestAdmin.Program
    main =
        PostgRestAdmin.configuration
            |> PostgRestAdmin.withMountPath "/back-office"
            |> PostgRestAdmin.buildProgram

-}
withMountPath : String -> Config msg -> Config msg
withMountPath p (Config conf) =
    Config { conf | mountPath = MountPath.fromString p }


mountPathDecoder : Decoder (Params msg) -> Decoder (Params msg)
mountPathDecoder =
    Flag.string "mountPath"
        (\p conf -> Decode.succeed { conf | mountPath = MountPath.fromString p })


{-| Set the number of records to display per page in listing views.

    main : PostgRestAdmin.Program
    main =
        PostgRestAdmin.configuration
            |> PostgRestAdmin.withRecordsPerPage 100
            |> PostgRestAdmin.buildProgram

-}
withRecordsPerPage : Int -> Config msg -> Config msg
withRecordsPerPage count (Config conf) =
    Config { conf | recordsPerPage = count }


recordsPerPageDecoder : Decoder (Params msg) -> Decoder (Params msg)
recordsPerPageDecoder =
    Flag.int "recordsPerPage"
        (\count conf -> Decode.succeed { conf | recordsPerPage = count })


{-| Pass a list of links to display in the side menu. The list consists of
tuples of the link text and a url.

    main : PostgRestAdmin.Program
    main =
        PostgRestAdmin.configuration
            |> PostgRestAdmin.withMenuLinks [ ( "Api Docs", "/api/docs" ) ]
            |> PostgRestAdmin.buildProgram

-}
withMenuLinks : List ( String, String ) -> Config msg -> Config msg
withMenuLinks links (Config conf) =
    Config { conf | menuLinks = links }


menuLinksDecoder : Decoder (Params msg) -> Decoder (Params msg)
menuLinksDecoder =
    Flag.linksList "menuLinks"
        (\links conf -> Decode.succeed { conf | menuLinks = links })


{-| Specify which fields should be present in the edit and create forms,
overriding the table schema. By default a primary key field is not present in
the forms.

    main : PostgRestAdmin.Program
    main =
        PostgRestAdmin.configuration
            |> PostgRestAdmin.withFormFields "posts" [ "id", "title", "content" ]
            |> PostgRestAdmin.buildProgram

-}
withFormFields : String -> List String -> Config msg -> Config msg
withFormFields tableName fields (Config conf) =
    Config
        { conf
            | formFields = Dict.insert tableName fields conf.formFields
        }


formFieldsDecoder : Decoder (Params msg) -> Decoder (Params msg)
formFieldsDecoder =
    Flag.stringListDict "formFields"
        (\fields conf ->
            Decode.succeed { conf | formFields = Dict.union fields conf.formFields }
        )


{-| Specify action buttons to be shown in the detail page of a
record along with Edit and Delete buttons.

`detailActions` expects a table name and a list of tuples. The first element of
each tuple is the button text and the second is a function that takes a record
and an ID and returns a URL string.

    import Url.Builder as Url

    main : PostgRestAdmin.Program
    main =
        PostgRestAdmin.configuration
            |> PostgRestAdmin.withDetailActions "posts"
                [ ( "View Comments"
                  , \_ id -> Url.absolute [ "posts", id, "comments" ] []
                  )
                ]
            |> PostgRestAdmin.buildProgram

-}
withDetailActions :
    String
    -> List ( String, Record -> String -> String )
    -> Config msg
    -> Config msg
withDetailActions tableName actions (Config conf) =
    Config
        { conf
            | detailActions =
                Dict.insert tableName actions conf.detailActions
        }


{-| Pass a list of table names to restrict the editable resources, also sets the
order of the left resources menu.

    main : PostgRestAdmin.Program
    main =
        PostgRestAdmin.configuration
            |> PostgRestAdmin.withTables [ "posts", "comments" ]
            |> PostgRestAdmin.buildProgram

-}
withTables : List String -> Config msg -> Config msg
withTables tableNames (Config conf) =
    Config { conf | tables = tableNames }


tablesDecoder : Decoder (Params msg) -> Decoder (Params msg)
tablesDecoder =
    Flag.stringList "tables"
        (\tableNames conf -> Decode.succeed { conf | tables = tableNames })


{-| Rename a table referenced in a foreign key. PostgREST OpenAPI generated docs
confuse tables with views when describing the foreign key for a resource,
because of this some links might be incorrectly generated.

    main : PostgRestAdmin.Program
    main =
        PostgRestAdmin.configuration
            |> PostgRestAdmin.withTableAliases
                (Dict.fromList [ ( "published_posts", "posts" ) ])
            |> PostgRestAdmin.buildProgram

-}
withTableAliases : Dict String String -> Config msg -> Config msg
withTableAliases aliases (Config conf) =
    Config { conf | tableAliases = aliases }


tableAliasesDecoder : Decoder (Params msg) -> Decoder (Params msg)
tableAliasesDecoder =
    Flag.stringDict "tableAliases"
        (\aliases conf -> Decode.succeed { conf | tableAliases = aliases })


{-| Display a banner text below the login form. The text is rendered as markdown.

    main : PostgRestAdmin.Program
    main =
        PostgRestAdmin.configuration
            |> PostgRestAdmin.withLoginBannerText "**Welcome!** Please login to continue."
            |> PostgRestAdmin.buildProgram

-}
withLoginBannerText : String -> Config msg -> Config msg
withLoginBannerText text (Config conf) =
    Config { conf | loginBannerText = Just text }


loginBannerTextDecoder : Decoder (Params msg) -> Decoder (Params msg)
loginBannerTextDecoder =
    Flag.string "loginBannerText"
        (\text conf -> Decode.succeed { conf | loginBannerText = Just text })
