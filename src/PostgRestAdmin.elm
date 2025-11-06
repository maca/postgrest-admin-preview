module PostgRestAdmin exposing
    ( Program
    , application
    )

{-|

@docs Program


# Init

@docs application

-}

import Browser
import Browser.Navigation as Nav
import Dict
import FormToolkit.Field as Field
import FormToolkit.Parse as Parse
import Html exposing (Html)
import Html.Attributes as Attrs
import Html.Events as Events
import Inflect as String
import Internal.Application as Application exposing (Application(..), Params)
import Internal.Client as Client
import Internal.Cmd as AppCmd
import Internal.Config as Config exposing (Config)
import Internal.Flag as Flag
import Internal.Http exposing (Error(..), Response, errorToString)
import Internal.Notification as Notification exposing (Notification)
import Internal.PageDetail as PageDetail exposing (PageDetail)
import Internal.PageForm as PageForm exposing (PageForm)
import Internal.PageListing as PageListing exposing (PageListing)
import Internal.Schema exposing (Schema)
import Json.Decode as Decode exposing (Decoder, Value)
import Json.Encode exposing (Value)
import Postgrest.Client as PG
import PostgRestAdmin.Client exposing (Client)
import PostgRestAdmin.MountPath as MountPath exposing (MountPath, path)
import String.Extra as String
import Task
import Url exposing (Url)
import Url.Builder as Url
import Url.Parser as Parser exposing ((</>), Parser, s)


{-| An alias to elm's Platform.Program providing the type signature for a
PostgRestAdmin program.
-}
type alias Program flags model msg =
    Platform.Program
        Decode.Value
        (Model flags model msg)
        (Msg flags model msg)


type alias AppParams f m msg =
    { init : Value -> Url.Url -> Nav.Key -> ( Model f m msg, Cmd (Msg f m msg) )
    , view : Model f m msg -> Browser.Document (Msg f m msg)
    , update : Msg f m msg -> Model f m msg -> ( Model f m msg, Cmd (Msg f m msg) )
    , subscriptions : Model f m msg -> Sub (Msg f m msg)
    , onUrlRequest : Browser.UrlRequest -> Msg f m msg
    , onUrlChange : Url -> Msg f m msg
    }


type alias InitParams f m msg =
    { client : Client
    , key : Nav.Key
    , config : Config f m msg
    }


type alias Model f m msg =
    { route : Route f m msg
    , key : Nav.Key
    , notification : Notification
    , error : Maybe Error
    , client : Client
    , onLogin : Maybe String -> Cmd (Msg f m msg)
    , mountedApp : Application.Application f m msg
    , mountedAppFlags : Result Decode.Error f
    , config : Config f m msg
    , attemptedPath : String
    }


type Msg f m msg
    = ApplicationInit ( Application.Params f m msg, msg )
    | AuthFieldsChanged (Field.Msg Never)
    | AuthFormSubmitted
    | GotToken (Result Client.AuthError Client.Session)
    | SchemaFetched (Result Error Schema)
    | PageListingChanged PageListing.Msg
    | PageDetailChanged PageDetail.Msg
    | PageFormChanged PageForm.Msg
    | PageApplicationChanged msg
    | RequestPerformed
        (Result Error Response
         -> Msg f m msg
        )
        (Result Error Response)
    | NotificationChanged Notification.Msg
    | LinkClicked Browser.UrlRequest
    | UrlChanged Url
    | LoggedIn { path : String, accessToken : String }
    | LoggedOut
    | NoOp


type Route f m msg
    = RouteRoot
    | RouteLoadingSchema
        (InitParams f m msg
         -> ( Route f m msg, Cmd (Msg f m msg) )
        )
    | RouteListing PageListing
    | RouteDetail PageDetail
    | RouteForm PageForm
    | RouteApplication
    | RouteNotFound


{-| Takes a Config and creates a PostgRestAdmin application.

See [Config](PostgRestAdmin.Config) to check all configuration
options.

    main : PostgRestAdmin.Program Never Never
    main =
        PostgRestAdmin.application Config.init

-}
application : Decoder (Config f m msg) -> Program f m msg
application decoder =
    Browser.application (applicationParams decoder)


applicationParams : Decoder (Config f m msg) -> AppParams f m msg
applicationParams decoder =
    { init =
        init
            (decoder
                |> Flag.string "host" Config.hostDecoder
                |> Flag.string "mountPath" Config.mountPathDecoder
                |> Flag.stringListDict "formFields" Config.formFieldsDecoder
                |> Flag.stringList "tables" Config.tablesDecoder
                |> Flag.stringDict "tableAliases" Config.tableAliasesDecoder
                |> Flag.linksList "menuLinks" Config.menuLinksDecoder
            )
    , update = update
    , view = view
    , subscriptions = subscriptions
    , onUrlRequest = LinkClicked
    , onUrlChange = UrlChanged
    }


init :
    Decoder (Config f m msg)
    -> Value
    -> Url.Url
    -> Nav.Key
    -> ( Model f m msg, Cmd (Msg f m msg) )
init decoder flags url key =
    let
        makeModel config =
            { route = RouteRoot
            , key = key
            , notification = Notification.none
            , error = Nothing
            , client = Client.init config.host config.authScheme
            , onLogin =
                Maybe.withDefault ""
                    >> config.onLogin
                    >> Cmd.map (always NoOp)
            , mountedApp = Application.none
            , mountedAppFlags = Decode.decodeValue config.flagsDecoder flags
            , config = config
            , attemptedPath = urlToPath url
            }
    in
    case Decode.decodeValue decoder flags of
        Ok config ->
            let
                model =
                    makeModel config

                ( route, cmd ) =
                    parseRoute url model
            in
            ( { model | route = route }, cmd )

        Err error ->
            let
                model =
                    makeModel Config.default
            in
            ( { model | error = Just (DecodeError error) }
            , Cmd.none
            )



-- UPDATE


update : Msg f m msg -> Model f m msg -> ( Model f m msg, Cmd (Msg f m msg) )
update msg model =
    let
        client =
            model.client
    in
    case msg of
        ApplicationInit ( params, childMsg ) ->
            let
                ( app, initCmd ) =
                    case model.mountedApp of
                        None ->
                            case model.mountedAppFlags of
                                Ok flags ->
                                    params.init
                                        { flags = flags
                                        , client = model.client
                                        , mountPath = model.config.mountPath
                                        }
                                        model.key
                                        |> Tuple.mapFirst (Application params)

                                Err err ->
                                    ( Application.DecodeFailed err
                                    , AppCmd.none
                                    )

                        _ ->
                            ( model.mountedApp, AppCmd.none )

                ( app_, cmd ) =
                    Application.update childMsg app
            in
            ( { model
                | mountedApp = app_
                , route = RouteApplication
              }
            , Cmd.batch
                [ mapAppCmd PageApplicationChanged initCmd
                , mapAppCmd PageApplicationChanged cmd
                ]
            )

        AuthFieldsChanged innerMsg ->
            case client.authScheme of
                Client.FormAuth data ->
                    ( { model
                        | client =
                            { client
                                | authScheme =
                                    Client.FormAuth
                                        { data
                                            | form = Field.update innerMsg data.form
                                            , status = Client.Active
                                        }
                            }
                      }
                    , Cmd.none
                    )

                _ ->
                    ( model, Cmd.none )

        AuthFormSubmitted ->
            case client.authScheme of
                Client.FormAuth data ->
                    ( model
                    , Task.attempt GotToken (Client.requestToken data)
                    )

                _ ->
                    ( model, Cmd.none )

        GotToken (Ok session) ->
            case client.authScheme of
                Client.FormAuth data ->
                    let
                        updatedClient =
                            { client
                                | authScheme =
                                    Client.FormAuth
                                        { data
                                            | form = Client.form
                                            , status = Client.Success session
                                        }
                            }

                        ( route, cmd ) =
                            case model.route of
                                RouteLoadingSchema func ->
                                    if Client.schemaIsLoaded updatedClient then
                                        func
                                            { client = updatedClient
                                            , key = model.key
                                            , config = model.config
                                            }

                                    else
                                        ( model.route
                                        , Task.attempt SchemaFetched
                                            (Client.fetchSchema model.config updatedClient)
                                        )

                                _ ->
                                    ( model.route, Cmd.none )
                    in
                    ( { model | client = updatedClient, route = route }
                    , Cmd.batch
                        [ loginCmd model updatedClient
                        , cmd
                        ]
                    )

                _ ->
                    ( model, Cmd.none )

        GotToken (Err error) ->
            case client.authScheme of
                Client.FormAuth data ->
                    ( { model
                        | client =
                            { client
                                | authScheme =
                                    Client.FormAuth { data | status = Client.Failure error }
                            }
                      }
                    , Cmd.none
                    )

                _ ->
                    ( model, Cmd.none )

        SchemaFetched (Ok schema) ->
            let
                updatedClient =
                    { client | schema = schema }

                ( route, cmd ) =
                    case model.route of
                        RouteLoadingSchema func ->
                            func
                                { client = updatedClient
                                , key = model.key
                                , config = model.config
                                }

                        _ ->
                            ( model.route, Cmd.none )
            in
            ( { model | client = updatedClient, route = route }
            , cmd
            )

        SchemaFetched (Err err) ->
            case err of
                AuthError ->
                    ( { model | client = Client.authFailed model.client }
                    , Cmd.none
                    )

                _ ->
                    ( model, Cmd.none )

        PageListingChanged childMsg ->
            case model.route of
                RouteListing listing ->
                    let
                        ( route, appCmd ) =
                            PageListing.update childMsg listing
                                |> Tuple.mapFirst RouteListing
                    in
                    ( { model | route = route }
                    , mapAppCmd PageListingChanged appCmd
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
                    , mapAppCmd PageDetailChanged cmd
                    )

                _ ->
                    ( model, Cmd.none )

        RequestPerformed mapper (Ok passedMsg) ->
            ( model
            , Task.attempt mapper (Task.succeed passedMsg)
            )

        RequestPerformed _ (Err AuthError) ->
            ( { model | client = Client.authFailed model.client }
            , Cmd.none
            )

        RequestPerformed mapper (Err err) ->
            ( model
            , Cmd.batch
                [ Task.attempt mapper (Task.fail err)
                , Notification.error (errorToString err)
                    |> Task.perform NotificationChanged
                ]
            )

        PageFormChanged childMsg ->
            case model.route of
                RouteForm prevForm ->
                    let
                        ( form, cmd ) =
                            PageForm.update childMsg prevForm
                    in
                    ( { model | route = RouteForm form }
                    , mapAppCmd PageFormChanged cmd
                    )

                _ ->
                    ( model, Cmd.none )

        PageApplicationChanged childMsg ->
            let
                ( app, cmd ) =
                    Application.update childMsg model.mountedApp
            in
            ( { model | mountedApp = app }
            , mapAppCmd PageApplicationChanged cmd
            )

        NotificationChanged childMsg ->
            let
                ( notification, cmd ) =
                    Notification.update childMsg
            in
            ( { model | notification = notification }
            , Cmd.map NotificationChanged cmd
            )

        LinkClicked urlRequest ->
            case urlRequest of
                Browser.Internal url ->
                    ( model
                    , Cmd.batch
                        [ Nav.pushUrl model.key (Url.toString url)
                        , Notification.dismiss
                            |> Task.perform NotificationChanged
                        ]
                    )

                Browser.External href ->
                    ( model, Nav.load href )

        UrlChanged url ->
            let
                ( route, cmd ) =
                    parseRoute url model
            in
            ( { model | route = route, attemptedPath = urlToPath url }
            , cmd
            )

        LoggedIn params ->
            ( { model
                | client = Client.updateJwt params.accessToken model.client
              }
            , Nav.pushUrl model.key params.path
            )

        LoggedOut ->
            ( { model | client = Client.logout model.client }
            , Cmd.map (always NoOp) (model.config.onLogout ())
            )

        NoOp ->
            ( model, Cmd.none )



--


loginCmd : Model f m msg -> Client -> Cmd (Msg f m msg)
loginCmd model client =
    let
        appLoginCmd =
            Cmd.batch
                [ model.onLogin (Client.toJwtString client)
                , case model.mountedApp of
                    Application params _ ->
                        Task.succeed (params.onLogin client)
                            |> Task.perform PageApplicationChanged

                    _ ->
                        Cmd.none
                ]
    in
    case model.route of
        RouteListing _ ->
            Cmd.batch
                [ Task.perform PageListingChanged
                    (Task.succeed (PageListing.onLogin client))
                , appLoginCmd
                ]

        RouteDetail _ ->
            Cmd.batch
                [ Task.perform PageDetailChanged
                    (Task.succeed (PageDetail.onLogin client))
                , appLoginCmd
                ]

        RouteForm _ ->
            Cmd.batch
                [ Task.perform PageFormChanged
                    (Task.succeed (PageForm.onLogin client))
                , appLoginCmd
                ]

        _ ->
            appLoginCmd



-- VIEW


view : Model f m msg -> Browser.Document (Msg f m msg)
view model =
    { title = "Admin"
    , body =
        case model.error of
            Just error ->
                [ Html.h1 [] [ Html.text "Init failed" ]
                , Html.pre
                    [ Attrs.class "parse-errors" ]
                    [ Html.text (errorToString error) ]
                ]

            Nothing ->
                [ Html.div
                    []
                    [ Html.div
                        [ Attrs.class "main-container" ]
                        [ sideMenu model
                        , Html.div
                            [ Attrs.class "main-area" ]
                            [ Notification.view model.notification
                                |> Html.map NotificationChanged
                            , mainContent model
                            ]
                        ]
                    , viewAuthForm model.client
                    ]
                ]
    }


viewAuthForm : Client -> Html (Msg f m msg)
viewAuthForm { authScheme } =
    viewAuthScheme authScheme


viewAuthScheme : Client.AuthScheme -> Html (Msg f m msg)
viewAuthScheme authScheme =
    case authScheme of
        Client.FormAuth data ->
            viewFormAuth data

        Client.Unset ->
            Html.div
                [ Attrs.class "overlay overlay-waiting" ]
                [ Html.i [ Attrs.class "gg-spinner-alt" ] []
                ]

        Client.Jwt _ ->
            Html.text ""


viewFormAuth : Client.AuthForm -> Html (Msg f m msg)
viewFormAuth data =
    if requiresAuthentication data.status then
        Html.div
            [ Attrs.class "auth-modal overlay" ]
            [ Html.div
                [ Attrs.class "auth-form" ]
                [ errorMessage data.status
                , Html.form
                    [ Attrs.class "auth-form"
                    , Events.onSubmit AuthFormSubmitted
                    ]
                    [ Field.toHtml AuthFieldsChanged data.form
                    , Html.button
                        [ Attrs.disabled
                            (Parse.parse Parse.json data.form
                                |> Result.map (\_ -> False)
                                |> Result.withDefault True
                            )
                        ]
                        [ Html.text "Login" ]
                    ]
                ]
            ]

    else
        Html.text ""


requiresAuthentication : Client.AuthFormStatus -> Bool
requiresAuthentication status =
    case status of
        Client.Ready ->
            True

        Client.Active ->
            True

        Client.Failure _ ->
            True

        Client.Success _ ->
            False


errorMessage : Client.AuthFormStatus -> Html (Msg f m msg)
errorMessage status =
    case status of
        Client.Failure error ->
            case error of
                Client.Forbidden ->
                    errorWrapper
                        [ Html.text """You may have entered the wrong password,
                          please try again."""
                        ]

                Client.Unauthorized ->
                    errorWrapper
                        [ Html.text
                            "Please sign in to continue."
                        ]

                Client.ServerError statusCode ->
                    errorWrapper
                        [ Html.text "The server responded with an error: "
                        , Html.pre [] [ Html.text (String.fromInt statusCode) ]
                        ]

                Client.DecodeError err ->
                    errorWrapper
                        [ Html.text "There was an issue parsing the server response: "
                        , Html.pre [] [ Html.text (Decode.errorToString err) ]
                        ]

                Client.NetworkError ->
                    errorWrapper
                        [ Html.text """There was an issue reaching the server,
                          please try again later."""
                        ]

        _ ->
            Html.div
                [ Attrs.class "form-error-message"
                , Attrs.style "visibility" "hidden"
                ]
                []


errorWrapper : List (Html (Msg f m msg)) -> Html (Msg f m msg)
errorWrapper =
    Html.div [ Attrs.class "form-error-message" ]


sideMenu : Model f m msg -> Html (Msg f m msg)
sideMenu ({ config } as model) =
    Html.div
        [ Attrs.class "side-menu" ]
        [ Html.aside
            [ Attrs.class "resources-menu" ]
            [ Html.ul [] (List.map (menuItem config.mountPath) (resources model))
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


menuItem : MountPath -> String -> Html (Msg f m msg)
menuItem mountPath name =
    Html.li
        []
        [ Html.a
            [ Attrs.href (path mountPath name) ]
            [ Html.text (String.toTitleCase (String.humanize name)) ]
        ]


extraMenuItem : ( String, String ) -> Html (Msg f m msg)
extraMenuItem ( linkText, url ) =
    Html.li
        []
        [ Html.a
            [ Attrs.href url ]
            [ Html.text (String.toTitleCase linkText) ]
        ]


mainContent : Model f m msg -> Html (Msg f m msg)
mainContent model =
    case model.route of
        RouteRoot ->
            Html.text ""

        RouteLoadingSchema _ ->
            loading

        RouteListing listing ->
            Html.map PageListingChanged (PageListing.view listing)

        RouteDetail listing ->
            Html.map PageDetailChanged (PageDetail.view listing)

        RouteForm form ->
            Html.map PageFormChanged (PageForm.view form)

        RouteApplication ->
            case model.mountedApp of
                Application program childModel ->
                    Html.map PageApplicationChanged (program.view childModel)

                DecodeFailed err ->
                    Html.pre
                        [ Attrs.class "error" ]
                        [ Html.text (Decode.errorToString err) ]

                None ->
                    notFound

        RouteNotFound ->
            notFound


notFound : Html (Msg f m msg)
notFound =
    Html.text "Not found"


loading : Html (Msg f m msg)
loading =
    Html.text ""



-- SUBSCRIPTIONS


subscriptions : Model f m msg -> Sub (Msg f m msg)
subscriptions { mountedApp, route, config } =
    Sub.batch
        [ Sub.map PageApplicationChanged (Application.subscriptions mountedApp)
        , case route of
            RouteListing pageListing ->
                Sub.map PageListingChanged
                    (PageListing.subscriptions pageListing)

            _ ->
                Sub.none
        , Sub.map LoggedIn (config.onExternalLogin identity)
        ]



-- ROUTES


parseRoute : Url -> Model f m msg -> ( Route f m msg, Cmd (Msg f m msg) )
parseRoute url model =
    if Client.schemaIsLoaded model.client then
        routeCons url
            { client = model.client
            , key = model.key
            , config = model.config
            }

    else
        ( RouteLoadingSchema (routeCons url)
        , Task.attempt SchemaFetched
            (Client.fetchSchema model.config model.client)
        )


routeCons : Url -> InitParams f m msg -> ( Route f m msg, Cmd (Msg f m msg) )
routeCons url params =
    Parser.parse
        (List.foldr (\p acc -> s p </> acc)
            (routeParser url params)
            (MountPath.segments params.config.mountPath)
        )
        url
        |> Maybe.withDefault ( RouteNotFound, Cmd.none )


routeParser :
    Url
    -> InitParams f m msg
    -> Parser (( Route f m msg, Cmd (Msg f m msg) ) -> a) a
routeParser url params =
    let
        appRoutes =
            case params.config.application of
                Just ( appParams, parser ) ->
                    [ Parser.map
                        (\msg ->
                            ( RouteApplication
                            , Task.succeed ( appParams, msg )
                                |> Task.perform ApplicationInit
                            )
                        )
                        (Parser.map identity parser)
                    ]

                Nothing ->
                    []
    in
    Parser.oneOf
        (appRoutes
            ++ [ -- /
                 Parser.map
                    ( RouteRoot
                    , resources params
                        |> List.head
                        |> Maybe.map
                            (\p ->
                                Nav.pushUrl params.key
                                    (MountPath.path params.config.mountPath p)
                            )
                        |> Maybe.withDefault Cmd.none
                    )
                    Parser.top

               -- /posts/new
               , Parser.map (initNewForm params Nothing)
                    (Parser.string </> s "new")

               -- /posts/edit
               , Parser.map (initForm params)
                    (Parser.string </> Parser.string </> s "edit")

               -- /posts
               , Parser.map (initListing params url Nothing) Parser.string

               -- /posts/1
               , Parser.map (initDetail params)
                    (Parser.string </> Parser.string)

               -- /posts/1/comments
               , Parser.map
                    (\parentTable parentId ->
                        Just { tableName = parentTable, id = parentId }
                            |> initListing params url
                    )
                    (Parser.string </> Parser.string </> Parser.string)

               -- /posts/1/comments/new
               , Parser.map
                    (\parentTable parentId ->
                        Just { tableName = parentTable, id = parentId }
                            |> initNewForm params
                    )
                    (Parser.string
                        </> Parser.string
                        </> Parser.string
                        </> s "new"
                    )
               ]
        )


initListing :
    InitParams f m msg
    -> Url
    -> Maybe { tableName : String, id : String }
    -> String
    -> ( Route f m msg, Cmd (Msg f m msg) )
initListing params url parent tableName =
    case Client.getTable tableName params.client of
        Just table ->
            let
                listingParams =
                    { client = params.client
                    , mountPath = params.config.mountPath
                    , table = table
                    , parent = parent
                    }
            in
            PageListing.init listingParams url params.key
                |> Tuple.mapFirst RouteListing
                |> Tuple.mapSecond (mapAppCmd PageListingChanged)

        Nothing ->
            ( RouteNotFound, Cmd.none )


initNewForm :
    InitParams f m msg
    -> Maybe { tableName : String, id : String }
    -> String
    -> ( Route f m msg, Cmd (Msg f m msg) )
initNewForm params parent tableName =
    initFormHelp params parent tableName Nothing


initForm :
    InitParams f m msg
    -> String
    -> String
    -> ( Route f m msg, Cmd (Msg f m msg) )
initForm params tableName id =
    initFormHelp params Nothing tableName (Just id)


initFormHelp :
    InitParams f m msg
    -> Maybe { tableName : String, id : String }
    -> String
    -> Maybe String
    -> ( Route f m msg, Cmd (Msg f m msg) )
initFormHelp { client, key, config } parent tableName id =
    case Client.getTable tableName client of
        Just table ->
            let
                params =
                    { client = client
                    , mountPath = config.mountPath
                    , fieldNames =
                        Dict.get tableName config.formFields
                            |> Maybe.withDefault []
                    , parent = parent
                    , table = table
                    , id = id
                    }
            in
            PageForm.init params key
                |> Tuple.mapFirst RouteForm
                |> Tuple.mapSecond (mapAppCmd PageFormChanged)

        Nothing ->
            ( RouteNotFound, Cmd.none )


initDetail :
    InitParams f m msg
    -> String
    -> String
    -> ( Route f m msg, Cmd (Msg f m msg) )
initDetail { client, key, config } tableName id =
    case Client.getTable tableName client of
        Just table ->
            let
                detailParams =
                    { client = client
                    , mountPath = config.mountPath
                    , table = table
                    , id = id
                    , detailActions =
                        config.detailActions
                            |> Dict.get tableName
                            |> Maybe.withDefault []
                    }
            in
            PageDetail.init detailParams key
                |> Tuple.mapFirst RouteDetail
                |> Tuple.mapSecond (mapAppCmd PageDetailChanged)

        Nothing ->
            ( RouteNotFound, Cmd.none )



-- UTILS


resources : { a | client : Client, config : Config f m msg } -> List String
resources { client, config } =
    if List.isEmpty config.tables then
        Dict.keys client.schema |> List.sort

    else
        config.tables


mapAppCmd : (a -> Msg f m b) -> AppCmd.Cmd a -> Cmd (Msg f m b)
mapAppCmd tagger appCmd =
    case appCmd of
        AppCmd.ChildCmd cmd ->
            Cmd.map tagger cmd

        AppCmd.Batch cmds ->
            Cmd.batch (List.map (mapAppCmd tagger) cmds)

        AppCmd.ChangeNotification cmd ->
            Cmd.map NotificationChanged cmd

        AppCmd.Fetch mapper task ->
            Task.attempt (RequestPerformed (mapper >> tagger)) task


urlToPath : Url -> String
urlToPath url =
    [ Just url.path, url.query ]
        |> List.filterMap identity
        |> String.join "?"
