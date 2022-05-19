port module PostgrestAdmin exposing (Program, application, applicationParams)

import Browser
import Browser.Navigation as Nav
import Detail exposing (Detail)
import Dict exposing (Dict)
import Dict.Extra as Dict
import FormPage exposing (Form)
import Html exposing (Html, a, aside, div, h1, li, pre, text, ul)
import Html.Attributes exposing (class, href)
import Inflect as String
import Json.Decode as Decode exposing (Decoder, Value)
import ListingPage exposing (Listing)
import Notification exposing (Notification)
import Postgrest.Client as PG
import Postgrest.Record as Record exposing (Record)
import Postgrest.Record.Client as Client exposing (Client)
import Postgrest.Schema as Schema exposing (Schema, Table)
import PostgrestAdmin.AuthScheme as AuthScheme
import PostgrestAdmin.Config as Config exposing (Config)
import PostgrestAdmin.OuterMsg as OuterMsg exposing (OuterMsg)
import PostgrestAdmin.Route as Route exposing (Route(..))
import String.Extra as String
import Url exposing (Url)
import Url.Parser as Parser exposing ((</>), Parser, s)
import Utils.Task exposing (Error(..), attemptWithError, errorToString, fail)


port loginSuccess : String -> Cmd msg


type alias Program model msg =
    Platform.Program Decode.Value (Model model msg) (Msg msg)


type Msg msg
    = SchemaFetched Schema
    | RecordFetched Record
    | ListingChanged ListingPage.Msg
    | DetailChanged Detail.Msg
    | FormChanged FormPage.Msg
    | AuthChanged AuthScheme.Msg
    | NotificationChanged Notification.Msg
    | CustomChanged msg
    | LinkClicked Browser.UrlRequest
    | UrlChanged Url
    | Failed Error


type alias Params m msg =
    { init : Value -> Url.Url -> Nav.Key -> ( Model m msg, Cmd (Msg msg) )
    , view : Model m msg -> Browser.Document (Msg msg)
    , update : Msg msg -> Model m msg -> ( Model m msg, Cmd (Msg msg) )
    , subscriptions : Model m msg -> Sub (Msg msg)
    , onUrlRequest : Browser.UrlRequest -> Msg msg
    , onUrlChange : Url -> Msg msg
    }


type alias Model m msg =
    Client
        { route : Route m msg
        , key : Nav.Key
        , notification : Notification
        , error : Maybe Error
        , formFields : Dict String (List String)
        }


application : Decoder Config -> Program m msg
application decoder =
    applicationParams decoder |> Browser.application


applicationParams : Decoder Config -> Params m msg
applicationParams decoder =
    { init = init decoder
    , update = update
    , view = view
    , subscriptions = subscriptions
    , onUrlRequest = LinkClicked
    , onUrlChange = UrlChanged
    }


init :
    Decoder Config
    -> Value
    -> Url.Url
    -> Nav.Key
    -> ( Model m msg, Cmd (Msg msg) )
init decoder flags url key =
    let
        makeModel config =
            { route = RouteRoot
            , key = key
            , schema = Dict.empty
            , notification = Notification.none
            , error = Nothing
            , host = config.url
            , authScheme = config.authScheme
            , formFields = config.formFields
            }
    in
    case Decode.decodeValue decoder flags of
        Ok config ->
            let
                model =
                    makeModel config
            in
            ( { model | route = getRoute url model }, fetchSchema model )

        Err error ->
            let
                model =
                    makeModel Config.default
            in
            ( { model | error = Just (DecodeError error) }, Cmd.none )



-- Update


update : Msg msg -> Model m msg -> ( Model m msg, Cmd (Msg msg) )
update msg model =
    case msg of
        SchemaFetched schema ->
            case model.route of
                RouteLoadingSchema routeCons ->
                    navigate
                        { model | schema = schema, route = routeCons schema }

                _ ->
                    ( model, Cmd.none )

        RecordFetched record ->
            case model.route of
                RouteFormLoading _ routeCons ->
                    navigate { model | route = routeCons record }

                _ ->
                    ( model, Cmd.none )

        ListingChanged childMsg ->
            case model.route of
                RouteListing listing ->
                    ListingPage.update model childMsg listing
                        |> updateRoute RouteListing ListingChanged model
                        |> handleChildMsg (ListingPage.mapMsg childMsg)

                _ ->
                    ( model, Cmd.none )

        DetailChanged childMsg ->
            case model.route of
                RouteDetail form ->
                    Detail.update model childMsg form
                        |> updateRoute RouteDetail DetailChanged model
                        |> handleChildMsg (Detail.mapMsg childMsg)

                _ ->
                    ( model, Cmd.none )

        FormChanged childMsg ->
            case model.route of
                RouteForm form ->
                    FormPage.update model childMsg form
                        |> updateRoute RouteForm FormChanged model
                        |> handleChildMsg (FormPage.mapMsg childMsg)

                _ ->
                    ( model, Cmd.none )

        AuthChanged childMsg ->
            let
                ( authScheme, cmd ) =
                    AuthScheme.update childMsg model.authScheme
            in
            handleChildMsg (AuthScheme.mapMsg childMsg)
                ( { model | authScheme = authScheme }
                , Cmd.map AuthChanged cmd
                )

        CustomChanged childMsg ->
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
            navigate { model | route = getRoute url model, error = Nothing }

        Failed err ->
            ( failed err model, Cmd.none )


navigate : Model m msg -> ( Model m msg, Cmd (Msg msg) )
navigate model =
    case model.route of
        RouteLoadingSchema routeCons ->
            if model.schema == Dict.empty then
                ( model, fetchSchema model )

            else
                navigate { model | route = routeCons model.schema }

        RouteFormLoading { tableName, id } routeCons ->
            case Dict.get tableName model.schema of
                Just table ->
                    ( model
                    , Client.fetch model table id
                        |> attemptWithError Failed RecordFetched
                    )

                Nothing ->
                    ( model, fail Failed (BadSchema tableName) )

        RouteListing listing ->
            ( listing, ListingPage.fetch model listing )
                |> updateRoute RouteListing ListingChanged model

        _ ->
            ( model, Cmd.none )



-- HTTP


fetchSchema : Model m msg -> Cmd (Msg msg)
fetchSchema { host } =
    Schema.fetchSchema host
        |> attemptWithError Failed SchemaFetched


updateRoute :
    (a -> Route m msg)
    -> (childMsg -> Msg msg)
    -> Model m msg
    -> ( a, Cmd childMsg )
    -> ( Model m msg, Cmd (Msg msg) )
updateRoute makeRoute makeMsg model ( a, cmd ) =
    ( { model | route = makeRoute a }
    , Cmd.map makeMsg cmd
    )


handleChildMsg :
    OuterMsg
    -> ( Model m msg, Cmd (Msg msg) )
    -> ( Model m msg, Cmd (Msg msg) )
handleChildMsg msg ( model, cmd ) =
    case msg of
        OuterMsg.RequestFailed err ->
            ( failed err model, Cmd.none )

        OuterMsg.NotificationChanged childMsg ->
            ( { model | notification = Notification.update childMsg }
            , Cmd.none
            )

        OuterMsg.LoginSuccess token ->
            let
                ( model_, cmd_ ) =
                    navigate model
            in
            ( model_, Cmd.batch [ cmd_, loginSuccess token ] )

        OuterMsg.Pass ->
            ( model, cmd )


failed : Error -> Model m msg -> Model m msg
failed error ({ authScheme } as model) =
    case error of
        PGError (PG.BadStatus 401 _ _) ->
            { model | authScheme = AuthScheme.fail authScheme }

        AuthError ->
            { model | authScheme = AuthScheme.fail authScheme }

        _ ->
            { model | error = Just error }



-- View


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
                    [ if AuthScheme.isAuthenticated model.authScheme then
                        div
                            [ class "main-container" ]
                            [ sideMenu model
                            , div [ class "main-area" ] (body model)
                            ]

                      else
                        AuthScheme.view model.authScheme |> Html.map AuthChanged
                    ]
                ]
    }


body : Model m msg -> List (Html (Msg msg))
body model =
    [ Notification.view model.notification
        |> Html.map NotificationChanged
    , mainContent model
    ]


sideMenu : Model m msg -> Html (Msg msg)
sideMenu model =
    aside
        [ class "resources-menu" ]
        [ ul [] (Dict.keys model.schema |> List.sort |> List.map menuItem) ]


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
            Html.map ListingChanged (ListingPage.view listing)

        RouteDetail listing ->
            Html.map DetailChanged (Detail.view model.schema listing)

        RouteFormLoading _ _ ->
            loading

        RouteForm form ->
            Html.map FormChanged (FormPage.view form)

        RouteNotFound ->
            notFound


notFound : Html (Msg msg)
notFound =
    text "Not found"


loading : Html (Msg msg)
loading =
    text ""



-- Subscriptions


subscriptions : Model m msg -> Sub (Msg msg)
subscriptions _ =
    Sub.none



-- Routes


getRoute : Url -> Model m msg -> Route m msg
getRoute url model =
    Parser.parse (routeParser url model) url |> Maybe.withDefault RouteNotFound


routeParser : Url -> Model m msg -> Parser (Route m msg -> a) a
routeParser url model =
    Parser.oneOf
        [ Parser.map RouteRoot Parser.top
        , Parser.map (newFormRoute model)
            (Parser.string </> s "new")
        , Parser.map (makeDetailRoute model)
            (Parser.string </> Parser.string)
        , Parser.map (editFormRoute model)
            (Parser.string </> Parser.string </> s "edit")
        , Parser.map (makeListingRoute model url)
            Parser.string
        ]


makeListingRoute : Model m msg -> Url -> String -> Route m msg
makeListingRoute model url tableName =
    RouteLoadingSchema
        (\schema ->
            case Dict.get tableName schema of
                Just table ->
                    let
                        setSearchVisibility =
                            case model.route of
                                RouteListing listing ->
                                    if ListingPage.isSearchVisible listing then
                                        ListingPage.showSearch

                                    else
                                        ListingPage.hideSearch

                                _ ->
                                    ListingPage.showSearch
                    in
                    ListingPage.init tableName url.query table
                        |> setSearchVisibility
                        |> RouteListing

                Nothing ->
                    RouteNotFound
        )


editFormRoute : Model m msg -> String -> String -> Route m msg
editFormRoute { formFields } resourcesName id =
    RouteFormLoading { tableName = resourcesName, id = id }
        (\record ->
            let
                fieldNames =
                    Dict.get resourcesName formFields |> Maybe.withDefault []
            in
            RouteForm
                (FormPage.init
                    { fieldNames = fieldNames
                    , id = Just id
                    , record = record
                    }
                )
        )
        |> always
        |> RouteLoadingSchema


newFormRoute : Model m msg -> String -> Route m msg
newFormRoute model tableName =
    RouteLoadingSchema
        (\schema ->
            case Dict.get tableName schema of
                Just table ->
                    let
                        fieldNames =
                            Dict.get tableName model.formFields
                                |> Maybe.withDefault []
                    in
                    RouteForm
                        (FormPage.init
                            { fieldNames = fieldNames
                            , id = Nothing
                            , record = Record.fromTable table
                            }
                        )

                Nothing ->
                    RouteNotFound
        )


makeDetailRoute : Model m msg -> String -> String -> Route m msg
makeDetailRoute { formFields } resourcesName id =
    RouteFormLoading { tableName = resourcesName, id = id }
        (Detail.init >> RouteDetail)
        |> always
        |> RouteLoadingSchema
