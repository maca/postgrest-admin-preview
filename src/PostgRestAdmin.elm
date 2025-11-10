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
import Dict exposing (Dict)
import FormToolkit.Field as Field
import FormToolkit.Parse as Parse
import Html exposing (Html)
import Html.Attributes as Attrs
import Html.Events as Events
import Http
import Internal.Application as Application exposing (Application(..))
import Internal.Cmd as AppCmd
import Internal.Config as Config exposing (Config)
import Internal.Flag as Flag
import Internal.Notification as Notification exposing (Notification)
import Internal.PageDetail as PageDetail exposing (PageDetail)
import Internal.PageForm as PageForm exposing (PageForm)
import Internal.PageListing as PageListing exposing (PageListing)
import Internal.Schema exposing (Schema)
import Json.Decode as Decode exposing (Decoder)
import Json.Encode as Encode exposing (Value)
import PostgRestAdmin.Client as Client exposing (Client)
import PostgRestAdmin.MountPath as MountPath exposing (MountPath, path)
import Postgrest.Client as PG
import String.Extra as String
import Task
import Url exposing (Protocol(..), Url)
import Url.Parser as Parser exposing ((</>), Parser, s)



-- AUTH FORM TYPES


type AuthFormStatus
    = Ready
    | Active
    | Submitting
    | Failure Client.Error



-- PROGRAM


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
    , error : Maybe String
    , client : Client
    , onLogin : Maybe String -> Cmd (Msg f m msg)
    , mountedApp : Application.Application f m msg
    , mountedAppFlags : Result Decode.Error f
    , config : Config f m msg
    , attemptedPath : String
    , authFormUrl : Url
    , authFormJwtDecoder : Decoder String
    , authFormJwtEncoder : Dict String String -> Encode.Value
    , authFormField : Field.Field Never
    , authFormStatus : AuthFormStatus
    }


type Msg f m msg
    = ApplicationInit ( Application.Params f m msg, msg )
    | AuthFieldsChanged (Field.Msg Never)
    | AuthFormSubmitted
    | GotToken (Result Client.Error String)
    | SchemaFetched (Result Client.Error Schema)
    | PageListingChanged PageListing.Msg
    | PageDetailChanged PageDetail.Msg
    | PageFormChanged PageForm.Msg
    | PageApplicationChanged msg
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

See [Config](PostgRestAdmin-Config) to check all configuration
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



-- INIT


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


requestToken : Model f m msg -> Cmd (Msg f m msg)
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
            , resolver = Http.stringResolver (handleAuthResponse model.authFormJwtDecoder)
            , timeout = Nothing
            }
        )


handleAuthResponse : Decoder a -> Http.Response String -> Result Client.Error a
handleAuthResponse aDecoder response =
    case response of
        Http.BadStatus_ { statusCode } _ ->
            if statusCode == 401 then
                Err Client.Unauthorized

            else if statusCode == 403 then
                Err Client.Forbidden

            else
                Err (Client.BadStatus statusCode)

        Http.GoodStatus_ _ body ->
            case Decode.decodeString aDecoder body of
                Err err ->
                    -- Decode error during auth
                    Err (Client.DecodeError err)

                Ok result ->
                    Ok result

        _ ->
            Err Client.NetworkError


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
            , authFormUrl =
                { protocol = Http
                , host = "localhost"
                , port_ = Just 9080
                , path = "/rpc/login"
                , query = Nothing
                , fragment = Nothing
                }
            , authFormJwtDecoder = Decode.field "token" Decode.string
            , authFormJwtEncoder = Encode.dict identity Encode.string
            , authFormField = authFormField
            , authFormStatus = Ready
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
            ( { model | error = Just (Decode.errorToString error) }
            , Cmd.none
            )



-- UPDATE


update : Msg f m msg -> Model f m msg -> ( Model f m msg, Cmd (Msg f m msg) )
update msg model =
    let
        client =
            model.client
    in
    case ( msg, model.route ) of
        ( ApplicationInit ( params, childMsg ), _ ) ->
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

        ( AuthFieldsChanged innerMsg, _ ) ->
            ( { model
                | authFormField = Field.update innerMsg model.authFormField
                , authFormStatus = Active
              }
            , Cmd.none
            )

        ( AuthFormSubmitted, _ ) ->
            ( { model | authFormStatus = Submitting }
            , requestToken model
            )

        ( GotToken (Ok tokenStr), RouteLoadingSchema func ) ->
            let
                updatedClient =
                    { client | authScheme = Client.Jwt (PG.jwt tokenStr) }

                ( route, cmd ) =
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
            in
            ( { model
                | client = updatedClient
                , route = route
              }
            , Cmd.batch [ loginCmd model updatedClient, cmd ]
            )

        ( GotToken (Ok tokenStr), _ ) ->
            let
                updatedClient =
                    { client | authScheme = Client.Jwt (PG.jwt tokenStr) }
            in
            ( { model | client = updatedClient }
            , loginCmd model updatedClient
            )

        ( GotToken (Err error), _ ) ->
            ( { model | authFormStatus = Failure error }
            , Cmd.none
            )

        ( SchemaFetched (Ok schema), RouteLoadingSchema func ) ->
            let
                updatedClient =
                    { client | schema = schema }

                ( route, cmd ) =
                    func
                        { client = updatedClient
                        , key = model.key
                        , config = model.config
                        }
            in
            ( { model | client = updatedClient, route = route }
            , cmd
            )

        ( SchemaFetched (Ok schema), _ ) ->
            ( { model | client = { client | schema = schema } }
            , Cmd.none
            )

        -- ( SchemaFetched (Err Internal.Http.AuthError), _ ) ->
        --     ( { model | client = Client.authFailed model.client }
        --     , Cmd.none
        --     )
        ( SchemaFetched (Err err), _ ) ->
            ( model
            , Notification.error (Client.errorToString err)
                |> Task.perform NotificationChanged
            )

        ( PageListingChanged childMsg, RouteListing listing ) ->
            let
                ( route, appCmd ) =
                    PageListing.update childMsg listing
                        |> Tuple.mapFirst RouteListing
            in
            ( { model | route = route }
            , mapAppCmd PageListingChanged appCmd
            )

        ( PageListingChanged _, _ ) ->
            ( model, Cmd.none )

        ( PageDetailChanged childMsg, RouteDetail prevDetail ) ->
            let
                ( detail, cmd ) =
                    PageDetail.update childMsg prevDetail
            in
            ( { model | route = RouteDetail detail }
            , mapAppCmd PageDetailChanged cmd
            )

        ( PageDetailChanged _, _ ) ->
            ( model, Cmd.none )

        ( PageFormChanged childMsg, RouteForm prevForm ) ->
            let
                ( form, cmd ) =
                    PageForm.update childMsg prevForm
            in
            ( { model | route = RouteForm form }
            , mapAppCmd PageFormChanged cmd
            )

        ( PageFormChanged _, _ ) ->
            ( model, Cmd.none )

        ( PageApplicationChanged childMsg, _ ) ->
            let
                ( app, cmd ) =
                    Application.update childMsg model.mountedApp
            in
            ( { model | mountedApp = app }
            , mapAppCmd PageApplicationChanged cmd
            )

        ( NotificationChanged childMsg, _ ) ->
            let
                ( notification, cmd ) =
                    Notification.update childMsg
            in
            ( { model | notification = notification }
            , Cmd.map NotificationChanged cmd
            )

        ( LinkClicked (Browser.Internal url), _ ) ->
            ( model
            , Cmd.batch
                [ Nav.pushUrl model.key (Url.toString url)
                , Notification.dismiss
                    |> Task.perform NotificationChanged
                ]
            )

        ( LinkClicked (Browser.External href), _ ) ->
            ( model, Nav.load href )

        ( UrlChanged url, _ ) ->
            let
                ( route, cmd ) =
                    parseRoute url model
            in
            ( { model | route = route, attemptedPath = urlToPath url }
            , cmd
            )

        ( LoggedIn params, _ ) ->
            ( { model | client = Client.updateJwt params.accessToken model.client }
            , Nav.pushUrl model.key params.path
            )

        ( LoggedOut, _ ) ->
            ( { model | client = Client.logout model.client }
            , Cmd.map (always NoOp) (model.config.onLogout ())
            )

        ( NoOp, _ ) ->
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
                    [ Html.text error ]
                ]

            Nothing ->
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
                                    [ Notification.view model.notification
                                        |> Html.map NotificationChanged
                                    , mainContent model
                                    ]
                                ]
                            ]
                        ]
    }


viewAuthForm : Model f m msg -> Html (Msg f m msg)
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
            ]
        ]


errorMessage : AuthFormStatus -> Html (Msg f m msg)
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

                    Client.BadStatus statusCode ->
                        [ Html.text "The server responded with an error: "
                        , Html.pre [] [ Html.text (String.fromInt statusCode) ]
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
            PageForm.init
                { client = client
                , navKey = key
                , mountPath = config.mountPath

                -- , fieldNames = Dict.get tableName config.formFields |> Maybe.withDefault []
                , parent = parent
                , table = table
                , id = id
                }
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


urlToPath : Url -> String
urlToPath url =
    [ Just url.path, url.query ]
        |> List.filterMap identity
        |> String.join "?"
