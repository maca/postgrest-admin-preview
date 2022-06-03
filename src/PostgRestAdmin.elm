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
import Dict.Extra as Dict
import Html exposing (Html, a, aside, div, h1, li, pre, text, ul)
import Html.Attributes exposing (class, href)
import Inflect as String
import Internal.Application as Application exposing (Application(..), Params)
import Internal.Client as Client
import Internal.Cmd as AppCmd
import Internal.Config as Config exposing (Config, MountPoint)
import Internal.Flag as Flag
import Internal.Msg exposing (Msg(..))
import Internal.Notification as Notification exposing (Notification)
import Internal.PageDetail as PageDetail exposing (PageDetail)
import Internal.PageForm as PageForm exposing (PageForm)
import Internal.PageListing as PageListing exposing (PageListing)
import Json.Decode as Decode exposing (Decoder, Value)
import PostgRestAdmin.Client exposing (Client)
import String.Extra as String
import Task
import Url exposing (Url)
import Url.Parser as Parser exposing ((</>), Parser, s)
import Utils.Task exposing (Error(..), errorToString)


type alias InitParams m msg =
    { client : Client
    , formFields : Dict String (List String)
    , key : Nav.Key
    , application : Maybe (MountPoint m msg)
    }


type Route m msg
    = RouteRoot
    | RouteLoadingSchema (InitParams m msg -> ( Route m msg, Cmd (Msg m msg) ))
    | RouteListing PageListing
    | RouteDetail PageDetail
    | RouteForm PageForm
    | RouteApplication
    | RouteNotFound


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
    , formFields : Dict String (List String)
    , client : Client
    , onLogin : Maybe String -> Cmd (Msg m msg)
    , applicationConfig : Maybe (MountPoint m msg)
    , mountedApp : Application.Application m msg
    }


{-| Takes a Config and creates a PostgRestAdmin application.

See [Config](PostgRestAdmin.Config) to check all configuration
options.

      main : PostgRestAdmin.Program Never Never
      main =
          PostgRestAdmin.application Config.init

-}
application : Decoder (Config m msg) -> Program m msg
application decoder =
    Browser.application
        (decoder
            |> Flag.string "host" Config.withHostDecoder
            |> Flag.stringDict "formFields" Config.withFormFieldsDecoder
            |> applicationParams
        )


applicationParams : Decoder (Config m msg) -> Params m msg
applicationParams decoder =
    { init = init decoder
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
            , formFields = config.formFields
            , onLogin =
                Maybe.withDefault ""
                    >> config.onLogin
                    >> Cmd.map (always NoOp)
            , applicationConfig = config.application
            , mountedApp = Application.none
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
        ApplicationInit ( params, m ) cmds ->
            ( { model
                | mountedApp =
                    case model.mountedApp of
                        Application _ _ ->
                            model.mountedApp

                        None ->
                            Application params m
              }
            , Cmd.batch (List.map (mapAppCmd PageApplicationChanged) cmds)
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
                                    , formFields = model.formFields
                                    , key = model.key
                                    , application = model.applicationConfig
                                    }

                            else if Client.isAuthSuccessMsg childMsg then
                                ( model.route
                                , Cmd.map ClientChanged
                                    (Client.fetchSchema client)
                                )

                            else
                                ( model.route, Cmd.none )

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

            else
                ( { model | client = client, route = route }
                , Cmd.batch [ Cmd.map ClientChanged clientCmd, cmd ]
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
                    ( model, Nav.pushUrl model.key <| Url.toString url )

                Browser.External href ->
                    ( model, Nav.load href )

        UrlChanged url ->
            let
                ( route, cmd ) =
                    parseRoute url model
            in
            ( { model | route = route }, cmd )

        NoOp ->
            ( model, Cmd.none )



--


loginCmd : Model m msg -> Client -> Cmd (Msg m msg)
loginCmd model client =
    let
        appLoginCmd =
            case model.mountedApp of
                Application params _ ->
                    Task.succeed (params.onLogin client)
                        |> Task.perform PageApplicationChanged

                None ->
                    Cmd.none
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
                        , div [ class "main-area" ] (body model)
                        ]
                    , Html.map ClientChanged (Client.view model.client)
                    ]
                ]
    }


body : Model m msg -> List (Html (Msg m msg))
body model =
    [ Notification.view model.notification |> Html.map NotificationChanged
    , mainContent model
    ]


sideMenu : Model m msg -> Html (Msg m msg)
sideMenu model =
    aside
        [ class "resources-menu" ]
        [ ul
            []
            (Dict.keys (Client.toSchema model.client)
                |> List.sort
                |> List.map menuItem
            )
        ]


menuItem : String -> Html (Msg m msg)
menuItem name =
    li
        []
        [ a [ href <| "/" ++ name ] [ text <| String.humanize name ] ]


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
            Html.map PageDetailChanged
                (PageDetail.view (Client.toSchema model.client) listing)

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
subscriptions _ =
    Sub.none



-- ROUTES


parseRoute : Url -> Model m msg -> ( Route m msg, Cmd (Msg m msg) )
parseRoute url model =
    let
        initTuple params =
            Parser.parse (routeParser url params) url
                |> Maybe.withDefault ( RouteNotFound, Cmd.none )
    in
    if Client.schemaIsLoaded model.client then
        initTuple
            { client = model.client
            , formFields = model.formFields
            , key = model.key
            , application = model.applicationConfig
            }

    else
        ( RouteLoadingSchema initTuple
        , Cmd.map ClientChanged (Client.fetchSchema model.client)
        )


routeParser :
    Url
    -> InitParams m msg
    -> Parser (( Route m msg, Cmd (Msg m msg) ) -> a) a
routeParser url model =
    let
        mountPoint =
            case model.application of
                Just app ->
                    [ Parser.map (\cmd -> ( RouteApplication, cmd ))
                        (applicationParser model app)
                    ]

                Nothing ->
                    []
    in
    Parser.oneOf
        (mountPoint
            ++ [ Parser.map ( RouteRoot, Cmd.none ) Parser.top
               , Parser.map (initNewForm model) (Parser.string </> s "new")
               , Parser.map (initForm model)
                    (Parser.string </> Parser.string </> s "edit")
               , Parser.map (initDetail model)
                    (Parser.string </> Parser.string)
               , Parser.map (initListing model url) Parser.string
               ]
        )


initListing :
    InitParams m msg
    -> Url
    -> String
    -> ( Route m msg, Cmd (Msg m msg) )
initListing model url tableName =
    case Client.getTable tableName model.client of
        Just table ->
            let
                params =
                    { client = model.client
                    , table = table
                    }
            in
            PageListing.init params url model.key
                |> Tuple.mapFirst RouteListing
                |> Tuple.mapSecond (mapAppCmd PageListingChanged)

        Nothing ->
            ( RouteNotFound, Cmd.none )


initNewForm : InitParams m msg -> String -> ( Route m msg, Cmd (Msg m msg) )
initNewForm model tableName =
    initFormHelp model tableName Nothing


initForm :
    InitParams m msg
    -> String
    -> String
    -> ( Route m msg, Cmd (Msg m msg) )
initForm model tableName id =
    initFormHelp model tableName (Just id)


initFormHelp :
    InitParams m msg
    -> String
    -> Maybe String
    -> ( Route m msg, Cmd (Msg m msg) )
initFormHelp model tableName id =
    case Client.getTable tableName model.client of
        Just table ->
            let
                params =
                    { client = model.client
                    , fieldNames =
                        Dict.get tableName model.formFields
                            |> Maybe.withDefault []
                    , id = id
                    , table = table
                    }
            in
            PageForm.init params model.key
                |> Tuple.mapFirst RouteForm
                |> Tuple.mapSecond (mapAppCmd PageFormChanged)

        Nothing ->
            ( RouteNotFound, Cmd.none )


initDetail :
    InitParams m msg
    -> String
    -> String
    -> ( Route m msg, Cmd (Msg m msg) )
initDetail model tableName id =
    case Client.getTable tableName model.client of
        Just table ->
            let
                params =
                    { client = model.client
                    , table = table
                    , id = id
                    }
            in
            PageDetail.init params model.key
                |> Tuple.mapFirst RouteDetail
                |> Tuple.mapSecond (mapAppCmd PageDetailChanged)

        Nothing ->
            ( RouteNotFound, Cmd.none )



-- MOUNTS


applicationParser :
    InitParams m msg
    -> MountPoint m msg
    -> Parser (Cmd (Msg m msg) -> a) a
applicationParser { client } ( program, parser ) =
    Parser.map
        (\msg ->
            let
                ( model, cmd ) =
                    program.init client
            in
            Task.succeed
                (ApplicationInit ( program, model )
                    [ Task.succeed msg
                        |> Task.perform identity
                        |> AppCmd.wrap
                    , cmd
                    ]
                )
                |> Task.perform identity
        )
        parser



-- UTILS


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
