module PostgRestAdmin exposing
    ( Config(..), Program, configure, buildProgram, buildAppParams
    , onLogin, onAuthFailed, onLogout, onExternalLogin
    , withHost, withLoginUrl, withJwt, withClientHeaders
    , withFlags
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
    , client : Client
    , currentUrl : Url
    , authFormUrl : Url
    , authFormJwtDecoder : Decoder String
    , authFormJwtEncoder : Dict String String -> Encode.Value
    , authFormField : Field.Field Never
    , authFormStatus : AuthFormStatus
    , onLogin : String -> Cmd Msg
    , mountPath : MountPath
    , menuLinks : List ( String, String )
    , loginBannerText : Maybe String
    , recordsPerPage : Int
    , configErrors : List String
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
    = Config (Params msg) (List String)


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
    Config defaultConfig []


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
        { init = \flags -> params.init (withFlags flags config)
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
        { init : Config msg -> Url -> Nav.Key -> ( model, Cmd outerMsg )
        , view : model -> Html outerMsg
        , update : outerMsg -> model -> ( model, Cmd outerMsg )
        , subscriptions : model -> Sub outerMsg
        , onUrlRequest : Browser.UrlRequest -> outerMsg
        , onUrlChange : Url -> outerMsg
        }
buildAppParams mappings (Config config _) =
    { init =
        \newConf url key ->
            let
                model =
                    initModel url key newConf

                (Config newParams _) =
                    newConf

                ( route, cmd ) =
                    parseRoute newParams model
                        |> Debug.log "init parse"
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
            >> view
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


initModel : Url -> Nav.Key -> Config msg -> Model
initModel url key (Config configRec errors) =
    { route = RouteRoot
    , key = key
    , notification = NoNotification
    , client =
        Client.init
            { host = configRec.host
            , authScheme = configRec.authScheme
            , headers = configRec.clientHeaders
            , tables = configRec.tables
            , tableAliases = configRec.tableAliases
            }
    , onLogin = configRec.onLogin >> Cmd.map (always NoOp)
    , currentUrl = url
    , authFormUrl = configRec.loginUrl
    , authFormJwtDecoder = Decode.field "token" Decode.string
    , authFormJwtEncoder = Encode.dict identity Encode.string
    , authFormField = authFormField
    , authFormStatus = Ready
    , mountPath = configRec.mountPath
    , menuLinks = configRec.menuLinks
    , loginBannerText = configRec.loginBannerText
    , recordsPerPage = configRec.recordsPerPage
    , configErrors = errors
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
update config msg ({ client } as model) =
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
                , Client.fetchSchema updatedClient
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

                updatedModel =
                    { model | client = updatedClient }

                ( route, cmd ) =
                    routeCons config updatedModel
            in
            ( { updatedModel | route = route }
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
                    updatedModel =
                        { model | currentUrl = url }

                    ( route, cmd ) =
                        parseRoute config updatedModel
                in
                ( { updatedModel | route = route }, cmd )

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


view : Model -> List (Html Msg)
view model =
    case model.configErrors of
        [] ->
            case model.client.authScheme of
                Client.Unset ->
                    [ viewAuthForm model ]

                _ ->
                    [ Html.div
                        []
                        [ Html.div
                            [ Attrs.class "main-container" ]
                            [ sideMenu model
                            , Html.div
                                [ Attrs.class "main-area" ]
                                [ viewNotification model.notification
                                , mainContent model
                                ]
                            ]
                        ]
                    ]

        errors ->
            [ Html.div
                [ Attrs.style "padding" "20px"
                , Attrs.style "font-family" "monospace"
                ]
                [ Html.h1 [] [ Html.text "Configuration Errors" ]
                , Html.ul
                    [ Attrs.style "color" "red"
                    , Attrs.style "line-height" "1.6"
                    ]
                    (List.map
                        (\error ->
                            Html.li [] [ Html.text error ]
                        )
                        errors
                    )
                ]
            ]


viewAuthForm : Model -> Html Msg
viewAuthForm model =
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
            , case model.loginBannerText of
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


sideMenu : Model -> Html Msg
sideMenu model =
    Html.div
        [ Attrs.class "side-menu" ]
        [ Html.aside
            [ Attrs.class "resources-menu" ]
            [ Html.ul [] (List.map (menuItem model.mountPath) (resources model.client))
            , Html.ul [] (List.map extraMenuItem model.menuLinks)
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
            Html.text "Not loaded"

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


parseRoute : Params msg -> Model -> ( Route, Cmd Msg )
parseRoute config model =
    if Client.toJwtString model.client == Nothing then
        ( RouteLoadingSchema model.currentUrl, Cmd.none )

    else if Client.schemaIsLoaded model.client then
        routeCons config model

    else
        ( RouteLoadingSchema model.currentUrl
        , Task.attempt SchemaFetched (Client.fetchSchema model.client)
        )


routeCons : Params msg -> Model -> ( Route, Cmd Msg )
routeCons config model =
    Parser.parse
        (List.foldr (\segment acc -> s segment </> acc)
            (routeParser config model)
            (MountPath.segments model.mountPath)
        )
        model.currentUrl
        |> Maybe.withDefault ( RouteNotFound, Cmd.none )


routeParser : Params msg -> Model -> Parser (( Route, Cmd Msg ) -> a) a
routeParser { detailActions } model =
    Parser.oneOf
        [ -- /
          Parser.map
            ( RouteRoot
            , resources model.client
                |> List.head
                |> Maybe.map
                    (\p ->
                        Nav.pushUrl model.key
                            (MountPath.path model.mountPath p)
                    )
                |> Maybe.withDefault Cmd.none
            )
            Parser.top

        -- /posts/new
        , Parser.map
            (\tableName ->
                initForm model
                    { parent = Nothing
                    , tableName = tableName
                    , id = Nothing
                    }
            )
            (Parser.string </> s "new")

        -- /posts/edit
        , Parser.map
            (\tableName id ->
                initForm model
                    { parent = Nothing
                    , tableName = tableName
                    , id = Just id
                    }
            )
            (Parser.string </> Parser.string </> s "edit")

        -- /posts
        , Parser.map
            (\tableName ->
                initListing model
                    { parent = Nothing
                    , tableName = tableName
                    }
            )
            Parser.string

        -- /posts/1
        , Parser.map
            (\tableName id ->
                initDetail model
                    { detailActions = detailActions
                    , tableName = tableName
                    , id = id
                    }
            )
            (Parser.string </> Parser.string)

        -- /posts/1/comments
        , Parser.map
            (\parentTable parentId tableName ->
                initListing model
                    { parent = Just { tableName = parentTable, id = parentId }
                    , tableName = tableName
                    }
            )
            (Parser.string </> Parser.string </> Parser.string)

        -- /posts/1/comments/new
        , Parser.map
            (\parentTable parentId tableName ->
                initForm model
                    { parent = Just { tableName = parentTable, id = parentId }
                    , tableName = tableName
                    , id = Nothing
                    }
            )
            (Parser.string </> Parser.string </> Parser.string </> s "new")
        ]


initListing :
    Model
    ->
        { parent : Maybe { tableName : String, id : String }
        , tableName : String
        }
    -> ( Route, Cmd Msg )
initListing model params =
    case Dict.get params.tableName model.client.schema of
        Just table ->
            let
                listingParams =
                    { client = model.client
                    , mountPath = model.mountPath
                    , table = table
                    , parent = params.parent
                    , recordsPerPage = model.recordsPerPage
                    }
            in
            PageListing.init listingParams model.currentUrl model.key
                |> Tuple.mapFirst RouteListing
                |> Tuple.mapSecond (mapCmd PageListingChanged)

        Nothing ->
            ( RouteNotFound, Cmd.none )


initForm :
    Model
    ->
        { parent : Maybe { tableName : String, id : String }
        , tableName : String
        , id : Maybe String
        }
    -> ( Route, Cmd Msg )
initForm model params =
    case Dict.get params.tableName model.client.schema of
        Just table ->
            PageForm.init
                { client = model.client
                , navKey = model.key
                , mountPath = model.mountPath
                , parent = params.parent
                , table = table
                , id = params.id
                }
                |> Tuple.mapFirst RouteForm
                |> Tuple.mapSecond (mapCmd PageFormChanged)

        Nothing ->
            ( RouteNotFound, Cmd.none )


initDetail :
    Model
    ->
        { detailActions : Dict String (List ( String, Record -> String -> String ))
        , tableName : String
        , id : String
        }
    -> ( Route, Cmd Msg )
initDetail model params =
    case Dict.get params.tableName model.client.schema of
        Just table ->
            let
                detailParams =
                    { client = model.client
                    , mountPath = model.mountPath
                    , table = table
                    , id = params.id
                    , detailActions =
                        params.detailActions
                            |> Dict.get params.tableName
                            |> Maybe.withDefault []
                    }
            in
            PageDetail.init detailParams model.key
                |> Tuple.mapFirst RouteDetail
                |> Tuple.mapSecond (mapCmd PageDetailChanged)

        Nothing ->
            ( RouteNotFound, Cmd.none )



-- UTILS


resources : Client -> List String
resources client =
    if List.isEmpty client.tables then
        Dict.keys client.schema |> List.sort

    else
        client.tables


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


configDecoder : Config msg -> Decoder (Config msg)
configDecoder config =
    Decode.succeed config
        |> Decode.andThen hostDecoder
        |> Decode.andThen loginUrlDecoder
        |> Decode.andThen jwtDecoder
        |> Decode.andThen clientHeadersDecoder
        |> Decode.andThen mountPathDecoder
        |> Decode.andThen recordsPerPageDecoder
        |> Decode.andThen menuLinksDecoder
        |> Decode.andThen formFieldsDecoder
        |> Decode.andThen tablesDecoder
        |> Decode.andThen tableAliasesDecoder
        |> Decode.andThen loginBannerTextDecoder


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
onLogin f (Config conf errors) =
    Config { conf | onLogin = f } errors


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
onAuthFailed f (Config conf errors) =
    Config { conf | onAuthFailed = f } errors


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
onExternalLogin sub (Config conf errors) =
    Config { conf | onExternalLogin = sub } errors


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
onLogout f (Config conf errors) =
    Config { conf | onLogout = f } errors


{-| Specify the PostgREST host.

    main : PostgRestAdmin.Program
    main =
        PostgRestAdmin.configuration
            |> PostgRestAdmin.withHost "http://localhost:3000"
            |> PostgRestAdmin.buildProgram

-}
withHost : String -> Config msg -> Config msg
withHost urlStr (Config conf errors) =
    case Url.fromString urlStr of
        Just u ->
            Config
                { conf
                    | host = u
                    , loginUrl = { u | path = "/rpc/login" }
                }
                errors

        Nothing ->
            Config conf (("`Config.host` was given an invalid URL: " ++ urlStr) :: errors)


withFlags : Decode.Value -> Config msg -> Config msg
withFlags value config =
    case Decode.decodeValue (configDecoder config) value of
        Ok newConfig ->
            newConfig

        Err err ->
            let
                (Config params errs) =
                    config
            in
            Config params (Decode.errorToString err :: errs)


hostDecoder : Config msg -> Decoder (Config msg)
hostDecoder config =
    Decode.maybe (Decode.field "host" Decode.string)
        |> Decode.map
            (Maybe.map (\urlStr -> withHost urlStr config)
                >> Maybe.withDefault config
            )


{-| Specify the login URL for form authentication.

    main : PostgRestAdmin.Program
    main =
        PostgRestAdmin.configuration
            |> PostgRestAdmin.withLoginUrl "http://localhost:3000/rpc/login"
            |> PostgRestAdmin.buildProgram

-}
withLoginUrl : String -> Config msg -> Config msg
withLoginUrl urlStr (Config conf errors) =
    case Url.fromString urlStr of
        Just u ->
            Config { conf | loginUrl = u } errors

        Nothing ->
            Config conf (("`Config.loginUrl` was given an invalid URL: " ++ urlStr) :: errors)


loginUrlDecoder : Config msg -> Decoder (Config msg)
loginUrlDecoder config =
    Decode.maybe (Decode.field "loginUrl" Decode.string)
        |> Decode.map
            (Maybe.map (\urlStr -> withLoginUrl urlStr config)
                >> Maybe.withDefault config
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
withJwt tokenStr (Config conf errors) =
    Config { conf | authScheme = Client.jwt tokenStr } errors


jwtDecoder : Config msg -> Decoder (Config msg)
jwtDecoder config =
    Decode.maybe (Decode.field "jwt" Decode.string)
        |> Decode.map
            (Maybe.map (\tokenStr -> withJwt tokenStr config)
                >> Maybe.withDefault config
            )


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
withClientHeaders headers (Config conf errors) =
    Config { conf | clientHeaders = headers } errors


clientHeadersDecoder : Config msg -> Decoder (Config msg)
clientHeadersDecoder config =
    Decode.maybe
        (Decode.field "clientHeaders" (Decode.dict Decode.string)
            |> Decode.map
                (\dict ->
                    Dict.toList dict
                        |> List.map (\( name, value ) -> Http.header name value)
                )
        )
        |> Decode.map
            (Maybe.map (\headers -> withClientHeaders headers config)
                >> Maybe.withDefault config
            )


{-| Specify a path prefix for all routes, in case the app is not mounted in the
root path.

    main : PostgRestAdmin.Program
    main =
        PostgRestAdmin.configuration
            |> PostgRestAdmin.withMountPath "/back-office"
            |> PostgRestAdmin.buildProgram

-}
withMountPath : String -> Config msg -> Config msg
withMountPath p (Config conf errors) =
    Config { conf | mountPath = MountPath.fromString p } errors


mountPathDecoder : Config msg -> Decoder (Config msg)
mountPathDecoder config =
    Decode.maybe (Decode.field "mountPath" Decode.string)
        |> Decode.map
            (Maybe.map (\p -> withMountPath p config)
                >> Maybe.withDefault config
            )


{-| Set the number of records to display per page in listing views.

    main : PostgRestAdmin.Program
    main =
        PostgRestAdmin.configuration
            |> PostgRestAdmin.withRecordsPerPage 100
            |> PostgRestAdmin.buildProgram

-}
withRecordsPerPage : Int -> Config msg -> Config msg
withRecordsPerPage count (Config conf errors) =
    Config { conf | recordsPerPage = count } errors


recordsPerPageDecoder : Config msg -> Decoder (Config msg)
recordsPerPageDecoder config =
    Decode.maybe (Decode.field "recordsPerPage" Decode.int)
        |> Decode.map
            (Maybe.map (\count -> withRecordsPerPage count config)
                >> Maybe.withDefault config
            )


{-| Pass a list of links to display in the side menu. The list consists of
tuples of the link text and a url.

    main : PostgRestAdmin.Program
    main =
        PostgRestAdmin.configuration
            |> PostgRestAdmin.withMenuLinks [ ( "Api Docs", "/api/docs" ) ]
            |> PostgRestAdmin.buildProgram

-}
withMenuLinks : List ( String, String ) -> Config msg -> Config msg
withMenuLinks links (Config conf errors) =
    Config { conf | menuLinks = links } errors


menuLinksDecoder : Config msg -> Decoder (Config msg)
menuLinksDecoder config =
    Decode.maybe
        (Decode.field "menuLinks"
            (Decode.list
                (Decode.map2 Tuple.pair
                    (Decode.field "text" Decode.string)
                    (Decode.field "url" Decode.string)
                )
            )
        )
        |> Decode.map
            (Maybe.map (\links -> withMenuLinks links config)
                >> Maybe.withDefault config
            )


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
withFormFields tableName fields (Config conf errors) =
    Config
        { conf | formFields = Dict.insert tableName fields conf.formFields }
        errors


formFieldsDecoder : Config msg -> Decoder (Config msg)
formFieldsDecoder config =
    Decode.maybe
        (Decode.field "formFields"
            (Decode.dict (Decode.list Decode.string))
        )
        |> Decode.map
            (Maybe.map (Dict.foldl withFormFields config)
                >> Maybe.withDefault config
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
withDetailActions table actions (Config conf errors) =
    Config { conf | detailActions = Dict.insert table actions conf.detailActions } errors


{-| Pass a list of table names to restrict the editable resources, also sets the
order of the left resources menu.

    main : PostgRestAdmin.Program
    main =
        PostgRestAdmin.configuration
            |> PostgRestAdmin.withTables [ "posts", "comments" ]
            |> PostgRestAdmin.buildProgram

-}
withTables : List String -> Config msg -> Config msg
withTables tableNames (Config conf errors) =
    Config { conf | tables = tableNames } errors


tablesDecoder : Config msg -> Decoder (Config msg)
tablesDecoder config =
    Decode.maybe (Decode.field "tables" (Decode.list Decode.string))
        |> Decode.map
            (Maybe.map (\tableNames -> withTables tableNames config)
                >> Maybe.withDefault config
            )


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
withTableAliases aliases (Config conf errors) =
    Config { conf | tableAliases = aliases } errors


tableAliasesDecoder : Config msg -> Decoder (Config msg)
tableAliasesDecoder config =
    Decode.maybe (Decode.field "tableAliases" (Decode.dict Decode.string))
        |> Decode.map
            (Maybe.map (\aliases -> withTableAliases aliases config)
                >> Maybe.withDefault config
            )


{-| Display a banner text below the login form. The text is rendered as markdown.

    main : PostgRestAdmin.Program
    main =
        PostgRestAdmin.configuration
            |> PostgRestAdmin.withLoginBannerText "**Welcome!** Please login to continue."
            |> PostgRestAdmin.buildProgram

-}
withLoginBannerText : String -> Config msg -> Config msg
withLoginBannerText text (Config conf errors) =
    Config { conf | loginBannerText = Just text } errors


loginBannerTextDecoder : Config msg -> Decoder (Config msg)
loginBannerTextDecoder config =
    Decode.maybe (Decode.field "loginBannerText" Decode.string)
        |> Decode.map
            (Maybe.map (\text -> withLoginBannerText text config)
                >> Maybe.withDefault config
            )
