module PostgrestAdmin exposing
    ( Program
    , application
    )

{-|


# Program configuration

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
import Internal.Client as Client
import Internal.Cmd as AppCmd
import Internal.Config as Config exposing (Config)
import Internal.Msg exposing (Msg(..))
import Internal.Notification as Notification exposing (Notification)
import Internal.PageDetail as PageDetail
import Internal.PageForm as PageForm
import Internal.PageListing as PageListing
import Internal.Route as Route exposing (MountPoint(..), Route(..))
import Json.Decode as Decode exposing (Decoder, Value)
import PostgrestAdmin.Client exposing (Client)
import String.Extra as String
import Task
import Url exposing (Url)
import Url.Parser as Parser exposing ((</>), Parser, s)
import Utils.Task exposing (Error(..), errorToString)


{-| An alias to elm's Platform.Program providing the type signature for a
PostgrestAdmin program.
-}
type alias Program model msg =
    Platform.Program Decode.Value (Model model msg) (Msg msg)


type alias Params m msg =
    { init : Value -> Url.Url -> Nav.Key -> ( Model m msg, Cmd (Msg msg) )
    , view : Model m msg -> Browser.Document (Msg msg)
    , update : Msg msg -> Model m msg -> ( Model m msg, Cmd (Msg msg) )
    , subscriptions : Model m msg -> Sub (Msg msg)
    , onUrlRequest : Browser.UrlRequest -> Msg msg
    , onUrlChange : Url -> Msg msg
    }


type alias Model m msg =
    { route : Route m msg
    , key : Nav.Key
    , notification : Notification
    , error : Maybe Error
    , formFields : Dict String (List String)
    , application : Maybe (MountPoint m msg)
    , client : Client
    , onLogin : Maybe String -> Cmd (Msg msg)
    }


{-| Takes a Config and creates a PostgrestAdmin application.
See [Config](PostgrestAdmin.Config) to check all configuration
options.

      main : PostgrestAdmin.Program Never Never
      main =
          PostgrestAdmin.application Config.init

-}
application : Decoder (Config m msg) -> Program m msg
application decoder =
    Browser.application (applicationParams decoder)


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
    -> ( Model m msg, Cmd (Msg msg) )
init decoder flags url key =
    let
        makeModel config =
            { route = RouteRoot
            , key = key
            , notification = Notification.none
            , error = Nothing
            , client = Client.init config.host config.authScheme
            , formFields = config.formFields
            , application = config.application
            , onLogin =
                Maybe.withDefault ""
                    >> config.onLogin
                    >> Cmd.map (always NoOp)
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


update : Msg msg -> Model m msg -> ( Model m msg, Cmd (Msg msg) )
update msg model =
    case msg of
        ClientChanged childMsg ->
            let
                ( client, clientCmd ) =
                    Client.update childMsg model.client
            in
            case model.route of
                RouteLoadingSchema func ->
                    let
                        ( route, cmd ) =
                            func
                                { client = client
                                , formFields = model.formFields
                                , key = model.key
                                , application = model.application
                                }
                    in
                    ( { model | client = client, route = route }
                    , Cmd.batch [ Cmd.map ClientChanged clientCmd, cmd ]
                    )

                RouteListing _ ->
                    model
                        |> clientChanged
                            { loginMsg = PageListing.onLogin
                            , tagger = PageListingChanged
                            , clientMsg = childMsg
                            }

                RouteDetail _ ->
                    model
                        |> clientChanged
                            { loginMsg = PageDetail.onLogin
                            , tagger = PageDetailChanged
                            , clientMsg = childMsg
                            }

                RouteForm _ ->
                    model
                        |> clientChanged
                            { loginMsg = PageForm.onLogin
                            , tagger = PageFormChanged
                            , clientMsg = childMsg
                            }

                RouteApplication program _ ->
                    model
                        |> clientChanged
                            { loginMsg = program.onLogin
                            , tagger = PageApplicationChanged
                            , clientMsg = childMsg
                            }

                _ ->
                    ( { model | client = client }
                    , Cmd.map ClientChanged clientCmd
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

        RequestPerformed client passedMsg ->
            ( { model | client = client }
            , Task.perform identity (Task.succeed passedMsg)
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
            case model.route of
                RouteApplication program prevChildModel ->
                    let
                        ( appModel, cmd ) =
                            program.update childMsg prevChildModel
                    in
                    ( { model | route = RouteApplication program appModel }
                    , mapAppCmd PageApplicationChanged cmd
                    )

                _ ->
                    ( model, Cmd.none )

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


clientChanged :
    { loginMsg : Client -> a
    , tagger : a -> Msg msg
    , clientMsg : Client.Msg
    }
    -> Model m msg
    -> ( Model m msg, Cmd (Msg msg) )
clientChanged { loginMsg, tagger, clientMsg } model =
    let
        ( client, clientCmd ) =
            Client.update clientMsg model.client
    in
    ( { model | client = client }
    , Cmd.batch
        [ Cmd.map ClientChanged clientCmd
        , if Client.isAuthSuccessMsg clientMsg then
            Cmd.batch
                [ loginMsg client
                    |> Task.succeed
                    |> Task.perform identity
                    |> Cmd.map tagger
                , model.onLogin (Client.toJwtString client)
                ]

          else
            Cmd.none
        ]
    )



-- VIEW


view : Model m msg -> Browser.Document (Msg msg)
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


body : Model m msg -> List (Html (Msg msg))
body model =
    [ Notification.view model.notification |> Html.map NotificationChanged
    , mainContent model
    ]


sideMenu : Model m msg -> Html (Msg msg)
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


menuItem : String -> Html (Msg msg)
menuItem name =
    li
        []
        [ a [ href <| "/" ++ name ] [ text <| String.humanize name ] ]


mainContent : Model m msg -> Html (Msg msg)
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

        RouteApplication program childModel ->
            Html.map PageApplicationChanged (program.view childModel)

        RouteNotFound ->
            notFound


notFound : Html (Msg msg)
notFound =
    text "Not found"


loading : Html (Msg msg)
loading =
    text ""



-- SUBSCRIPTIONS


subscriptions : Model m msg -> Sub (Msg msg)
subscriptions _ =
    Sub.none



-- ROUTES


parseRoute : Url -> Model m msg -> ( Route m msg, Cmd (Msg msg) )
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
            , application = model.application
            }

    else
        ( RouteLoadingSchema initTuple
        , Cmd.map ClientChanged (Client.fetchSchema model.client)
        )


routeParser :
    Url
    -> Route.InitParams m msg
    -> Parser (( Route m msg, Cmd (Msg msg) ) -> a) a
routeParser url model =
    let
        mountPoint =
            model.application
                |> Maybe.map (applicationParser model >> List.singleton)
                |> Maybe.withDefault []
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
    Route.InitParams m msg
    -> Url
    -> String
    -> ( Route m msg, Cmd (Msg msg) )
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


initNewForm : Route.InitParams m msg -> String -> ( Route m msg, Cmd (Msg msg) )
initNewForm model tableName =
    initFormHelp model tableName Nothing


initForm :
    Route.InitParams m msg
    -> String
    -> String
    -> ( Route m msg, Cmd (Msg msg) )
initForm model tableName id =
    initFormHelp model tableName (Just id)


initFormHelp :
    Route.InitParams m msg
    -> String
    -> Maybe String
    -> ( Route m msg, Cmd (Msg msg) )
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
    Route.InitParams m msg
    -> String
    -> String
    -> ( Route m msg, Cmd (Msg msg) )
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
    Route.InitParams m msg
    -> MountPoint m msg
    -> Parser (( Route m msg, Cmd (Msg msg) ) -> a) a
applicationParser { client } (MountPoint program parser) =
    Parser.map
        (\() ->
            program.init client
                |> Tuple.mapFirst (RouteApplication program)
                |> Tuple.mapSecond (mapAppCmd PageApplicationChanged)
        )
        parser



-- UTILS


mapAppCmd : (a -> Msg b) -> AppCmd.Cmd a -> Cmd (Msg b)
mapAppCmd tagger appCmd =
    case appCmd of
        AppCmd.ChildCmd cmd ->
            Cmd.map tagger cmd

        AppCmd.Fetch decode task ->
            task
                |> Task.map
                    (\client ->
                        tagger (decode (Client.toResponse client))
                            |> RequestPerformed client
                    )
                |> Task.perform identity
