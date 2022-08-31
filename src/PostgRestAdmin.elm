module PostgRestAdmin exposing
    ( Program
    , application
    , breadcrumbs
    )

{-|

@docs Program


# Init

@docs application
@docs breadcrumbs

-}

import Browser
import Browser.Navigation as Nav
import Dict
import Html exposing (Html, a, aside, button, div, h1, i, li, pre, text, ul)
import Html.Attributes exposing (class, href)
import Html.Events exposing (onClick)
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
import Internal.ViewHelp as ViewHelp
import Json.Decode as Decode exposing (Decoder, Value)
import Json.Encode exposing (Value)
import PostgRestAdmin.Client exposing (Client)
import String.Extra as String
import Task
import Url exposing (Url)
import Url.Builder as Url
import Url.Parser as Parser exposing ((</>), Parser, s)


{-| An alias to elm's Platform.Program providing the type signature for a
PostgRestAdmin program.
-}
type alias Program model msg =
    Platform.Program Decode.Value (Model model msg) (Msg model msg)


type alias Params m msg =
    { init : Value -> Url.Url -> Nav.Key -> ( Model m msg, Cmd (Msg m msg) )
    , view : Model m msg -> Browser.Document (Msg m msg)
    , update : Msg m msg -> Model m msg -> ( Model m msg, Cmd (Msg m msg) )
    , subscriptions : Model m msg -> Sub (Msg m msg)
    , onUrlRequest : Browser.UrlRequest -> Msg m msg
    , onUrlChange : Url -> Msg m msg
    }


type alias Model m msg =
    { route : Route m msg
    , key : Nav.Key
    , notification : Notification
    , error : Maybe Error
    , client : Client
    , onLogin : Maybe String -> Cmd (Msg m msg)
    , mountedApp : Application.Application m msg
    , config : Config m msg
    , attemptedPath : String
    }


type alias InitParams m msg =
    { client : Client
    , key : Nav.Key
    , config : Config m msg
    }


type Msg m msg
    = ApplicationInit ( Application.Params m msg, msg )
    | ClientChanged Client.Msg
    | PageListingChanged PageListing.Msg
    | PageDetailChanged PageDetail.Msg
    | PageFormChanged PageForm.Msg
    | PageApplicationChanged msg
    | RequestPerformed
        (Result Error Response
         -> Msg m msg
        )
        (Result Error Response)
    | NotificationChanged Notification.Msg
    | LinkClicked Browser.UrlRequest
    | UrlChanged Url
    | LoggedIn { path : String, accessToken : String }
    | LoggedOut
    | NoOp


type Route m msg
    = RouteRoot
    | RouteLoadingSchema (InitParams m msg -> ( Route m msg, Cmd (Msg m msg) ))
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
application : Decoder (Config m msg) -> Program m msg
application decoder =
    Browser.application (applicationParams decoder)


{-| -}
breadcrumbs : String -> List ( String, Maybe String ) -> Html msg
breadcrumbs =
    ViewHelp.breadcrumbs


applicationParams : Decoder (Config m msg) -> Params m msg
applicationParams decoder =
    { init =
        init
            (decoder
                |> Flag.string "host" Config.hostDecoder
                |> Flag.stringDict "formFields" Config.formFieldsDecoder
                |> Flag.stringList "tables" Config.tablesDecoder
            )
    , update = update
    , view = view
    , subscriptions = subscriptions
    , onUrlRequest = LinkClicked
    , onUrlChange = UrlChanged
    }


init :
    Decoder (Config m msg)
    -> Value
    -> Url.Url
    -> Nav.Key
    -> ( Model m msg, Cmd (Msg m msg) )
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


update : Msg m msg -> Model m msg -> ( Model m msg, Cmd (Msg m msg) )
update msg model =
    case msg of
        ApplicationInit ( params, childMsg ) ->
            let
                ( app, initCmd ) =
                    case model.mountedApp of
                        Application _ _ ->
                            ( model.mountedApp, AppCmd.none )

                        None ->
                            params.init model.client model.key
                                |> Tuple.mapFirst (Application params)

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

        ClientChanged childMsg ->
            let
                ( client, clientCmd ) =
                    Client.update childMsg model.client

                ( route, cmd ) =
                    case model.route of
                        RouteLoadingSchema func ->
                            if Client.schemaIsLoaded client then
                                func
                                    { client = client
                                    , key = model.key
                                    , config = model.config
                                    }

                            else
                                ( model.route
                                , Cmd.map ClientChanged
                                    (Client.fetchSchema model.config.tables
                                        client
                                    )
                                )

                        _ ->
                            ( model.route, Cmd.none )
            in
            if Client.isAuthSuccessMsg childMsg then
                ( { model | client = client, route = route }
                , Cmd.batch
                    [ Cmd.map ClientChanged clientCmd
                    , loginCmd model client
                    , cmd
                    ]
                )

            else if Client.isAuthenticated client then
                ( { model | client = client, route = route }
                , Cmd.batch [ Cmd.map ClientChanged clientCmd, cmd ]
                )

            else
                ( model
                , model.config.onAuthFailed model.attemptedPath
                    |> Cmd.map (always NoOp)
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
            ( { model | notification = Notification.update childMsg }
            , Cmd.none
            )

        LinkClicked urlRequest ->
            case urlRequest of
                Browser.Internal url ->
                    ( model, Nav.pushUrl model.key (Url.toString url) )

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

        LoggedIn { path, accessToken } ->
            ( { model | client = Client.updateJwt accessToken model.client }
            , Nav.pushUrl model.key path
            )

        LoggedOut ->
            ( { model | client = Client.logout model.client }
            , Cmd.map (always NoOp) (model.config.onLogout ())
            )

        NoOp ->
            ( model, Cmd.none )



--


loginCmd : Model m msg -> Client -> Cmd (Msg m msg)
loginCmd model client =
    let
        appLoginCmd =
            Cmd.batch
                [ model.onLogin (Client.toJwtString client)
                , case model.mountedApp of
                    Application params _ ->
                        Task.succeed (params.onLogin client)
                            |> Task.perform PageApplicationChanged

                    None ->
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


view : Model m msg -> Browser.Document (Msg m msg)
view model =
    { title = "Admin"
    , body =
        case model.error of
            Just error ->
                [ h1 [] [ text "Init failed" ]
                , pre
                    [ class "parse-errors" ]
                    [ text (errorToString error) ]
                ]

            Nothing ->
                [ div
                    []
                    [ div
                        [ class "main-container" ]
                        [ sideMenu model
                        , div
                            [ class "main-area" ]
                            [ Notification.view model.notification
                                |> Html.map NotificationChanged
                            , mainContent model
                            ]
                        ]
                    , Html.map ClientChanged (Client.view model.client)
                    ]
                ]
    }


sideMenu : Model m msg -> Html (Msg m msg)
sideMenu model =
    div
        [ class "side-menu" ]
        [ aside
            [ class "resources-menu" ]
            [ ul
                []
                (List.map menuItem (resources model))
            ]
        , div
            [ class "account-management" ]
            [ button
                [ onClick LoggedOut
                , class "button button-clear"
                ]
                [ i [ class "gg-log-out" ] []
                , text "Logout"
                ]
            ]
        ]


menuItem : String -> Html (Msg m msg)
menuItem name =
    li
        []
        [ a
            [ href <| "/" ++ name ]
            [ text (String.toTitleCase (String.humanize name)) ]
        ]


mainContent : Model m msg -> Html (Msg m msg)
mainContent model =
    case model.route of
        RouteRoot ->
            text ""

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

                None ->
                    notFound

        RouteNotFound ->
            notFound


notFound : Html (Msg m msg)
notFound =
    text "Not found"


loading : Html (Msg m msg)
loading =
    text ""



-- SUBSCRIPTIONS


subscriptions : Model m msg -> Sub (Msg m msg)
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


parseRoute : Url -> Model m msg -> ( Route m msg, Cmd (Msg m msg) )
parseRoute url model =
    let
        routeTuple params =
            Parser.parse (routeParser url params) url
                |> Maybe.withDefault ( RouteNotFound, Cmd.none )
    in
    if Client.schemaIsLoaded model.client then
        routeTuple
            { client = model.client
            , key = model.key
            , config = model.config
            }

    else
        ( RouteLoadingSchema routeTuple
        , Cmd.map ClientChanged
            (Client.fetchSchema model.config.tables model.client)
        )


routeParser :
    Url
    -> InitParams m msg
    -> Parser (( Route m msg, Cmd (Msg m msg) ) -> a) a
routeParser url params =
    let
        mountPoint =
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
        (mountPoint
            ++ [ -- /
                 Parser.map ( RouteRoot, Cmd.none ) Parser.top

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
    InitParams m msg
    -> Url
    -> Maybe { tableName : String, id : String }
    -> String
    -> ( Route m msg, Cmd (Msg m msg) )
initListing params url parent tableName =
    case Client.getTable tableName params.client of
        Just table ->
            let
                listingParams =
                    { client = params.client
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
    InitParams m msg
    -> Maybe { tableName : String, id : String }
    -> String
    -> ( Route m msg, Cmd (Msg m msg) )
initNewForm params parent tableName =
    initFormHelp params parent tableName Nothing


initForm :
    InitParams m msg
    -> String
    -> String
    -> ( Route m msg, Cmd (Msg m msg) )
initForm params tableName id =
    initFormHelp params Nothing tableName (Just id)


initFormHelp :
    InitParams m msg
    -> Maybe { tableName : String, id : String }
    -> String
    -> Maybe String
    -> ( Route m msg, Cmd (Msg m msg) )
initFormHelp { client, key, config } parent tableName id =
    case Client.getTable tableName client of
        Just table ->
            let
                params =
                    { client = client
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
    InitParams m msg
    -> String
    -> String
    -> ( Route m msg, Cmd (Msg m msg) )
initDetail { client, key, config } tableName id =
    case Client.getTable tableName client of
        Just table ->
            let
                detailParams =
                    { client = client
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


resources : Model m msg -> List String
resources { client, config } =
    if List.isEmpty config.tables then
        Dict.keys (Client.toSchema client) |> List.sort

    else
        config.tables


mapAppCmd : (a -> Msg m b) -> AppCmd.Cmd a -> Cmd (Msg m b)
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
