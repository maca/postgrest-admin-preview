port module PostgrestAdmin exposing (Model, Msg, application, applicationParams)

import Browser
import Browser.Navigation as Nav
import Detail exposing (Detail)
import Dict exposing (Dict)
import Dict.Extra as Dict
import Form exposing (Form)
import Html exposing (Html, a, aside, div, h1, li, pre, text, ul)
import Html.Attributes exposing (class, href)
import Inflect as String
import Json.Decode as Decode exposing (Decoder, Value)
import Listing exposing (Listing)
import Notification exposing (Notification)
import Postgrest.Client as PG
import Postgrest.Record.Client exposing (Client)
import Postgrest.Schema as Schema exposing (Schema, Table)
import PostgrestAdmin.AuthScheme as AuthScheme
import PostgrestAdmin.Config as Config exposing (Config)
import PostgrestAdmin.OuterMsg as OuterMsg exposing (OuterMsg)
import String.Extra as String
import Url exposing (Url)
import Url.Parser as Parser exposing ((</>), Parser, s)
import Utils.Task exposing (Error(..), attemptWithError, errorToString, fail)


port loginSuccess : String -> Cmd msg


type Route
    = RouteRoot
    | RouteLoadingTable String (Table -> Route)
    | RouteListing Listing
    | RouteDetail Detail
    | RouteFormLoading Form String
    | RouteForm Form
    | RouteNotFound


type Msg
    = SchemaFetched Schema
    | ListingChanged Listing.Msg
    | DetailChanged Detail.Msg
    | FormChanged Form.Msg
    | AuthChanged AuthScheme.Msg
    | NotificationChanged Notification.Msg
    | LinkClicked Browser.UrlRequest
    | UrlChanged Url
    | Failed Error


type alias Params =
    { init : Value -> Url.Url -> Nav.Key -> ( Model, Cmd Msg )
    , view : Model -> Browser.Document Msg
    , update : Msg -> Model -> ( Model, Cmd Msg )
    , subscriptions : Model -> Sub Msg
    , onUrlRequest : Browser.UrlRequest -> Msg
    , onUrlChange : Url -> Msg
    }


type alias Model =
    Client
        { route : Route
        , key : Nav.Key
        , notification : Notification
        , error : Maybe Error
        , formFields : Dict String (List String)
        }


application : Decoder Config -> Program Value Model Msg
application decoder =
    applicationParams decoder |> Browser.application


applicationParams : Decoder Config -> Params
applicationParams decoder =
    { init = init decoder
    , update = update
    , view = view
    , subscriptions = subscriptions
    , onUrlRequest = LinkClicked
    , onUrlChange = UrlChanged
    }


init : Decoder Config -> Value -> Url.Url -> Nav.Key -> ( Model, Cmd Msg )
init decoder flags url key =
    let
        makeModel config =
            { route = RouteRoot
            , key = key
            , schema = Dict.fromList []
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
            ( { model | route = getRoute url model }
            , Schema.getSchema model.host
                |> attemptWithError Failed SchemaFetched
            )

        Err error ->
            let
                model =
                    makeModel Config.default
            in
            ( { model | error = Just (DecodeError error) }, Cmd.none )



-- Update


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        SchemaFetched schema ->
            navigate { model | schema = schema }

        ListingChanged childMsg ->
            case model.route of
                RouteListing listing ->
                    Listing.update model childMsg listing
                        |> updateRoute RouteListing ListingChanged model
                        |> handleChildMsg (Listing.mapMsg childMsg)

                _ ->
                    ( model, Cmd.none )

        DetailChanged childMsg ->
            case model.route of
                RouteDetail form ->
                    Detail.update childMsg form
                        |> updateRoute RouteDetail DetailChanged model
                        |> handleChildMsg (Detail.mapMsg childMsg)

                _ ->
                    ( model, Cmd.none )

        FormChanged childMsg ->
            case model.route of
                RouteForm form ->
                    Form.update model childMsg form
                        |> updateRoute RouteForm FormChanged model
                        |> handleChildMsg (Form.mapMsg childMsg)

                RouteFormLoading form _ ->
                    Form.update model childMsg form
                        |> updateRoute RouteForm FormChanged model
                        |> handleChildMsg (Form.mapMsg childMsg)

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


navigate : Model -> ( Model, Cmd Msg )
navigate model =
    case model.route of
        RouteLoadingTable resourcesName makeRoute ->
            case Dict.get resourcesName model.schema of
                Just table ->
                    navigate { model | route = makeRoute table }

                Nothing ->
                    ( model, fail Failed <| BadSchema resourcesName )

        RouteListing listing ->
            ( listing, Listing.fetch model listing )
                |> updateRoute RouteListing ListingChanged model

        RouteDetail detail ->
            ( detail, Detail.fetch model detail )
                |> updateRoute RouteDetail DetailChanged model

        RouteFormLoading form id ->
            ( model, Form.fetch model form id |> Cmd.map FormChanged )

        _ ->
            ( model, Cmd.none )


updateRoute :
    (a -> Route)
    -> (childMsg -> Msg)
    -> Model
    -> ( a, Cmd childMsg )
    -> ( Model, Cmd Msg )
updateRoute makeRoute makeMsg model ( a, cmd ) =
    ( { model | route = makeRoute a }
    , Cmd.map makeMsg cmd
    )


handleChildMsg : OuterMsg -> ( Model, Cmd Msg ) -> ( Model, Cmd Msg )
handleChildMsg msg ( model, cmd ) =
    case msg of
        OuterMsg.RequestFailed err ->
            ( failed err model, Cmd.none )

        OuterMsg.NotificationChanged childMsg ->
            ( { model | notification = Notification.update childMsg }
            , Cmd.none
            )

        OuterMsg.LoginSuccess token ->
            ( model, Cmd.batch [ fetch model, loginSuccess token ] )

        OuterMsg.Pass ->
            ( model, cmd )


fetch : Model -> Cmd Msg
fetch model =
    case model.route of
        RouteListing listing ->
            Listing.fetch model listing |> Cmd.map ListingChanged

        RouteFormLoading form id ->
            Form.fetch model form id |> Cmd.map FormChanged

        RouteDetail detail ->
            Detail.fetch model detail |> Cmd.map DetailChanged

        RouteForm form ->
            case Form.id form of
                Just id ->
                    Form.fetch model form id |> Cmd.map FormChanged

                Nothing ->
                    Cmd.none

        _ ->
            Cmd.none


failed : Error -> Model -> Model
failed error ({ authScheme } as model) =
    case error of
        PGError (PG.BadStatus 401 _ _) ->
            { model | authScheme = AuthScheme.fail authScheme }

        AuthError ->
            { model | authScheme = AuthScheme.fail authScheme }

        _ ->
            { model | error = Just error }



-- View


view : Model -> Browser.Document Msg
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


body : Model -> List (Html Msg)
body model =
    [ Notification.view model.notification
        |> Html.map NotificationChanged
    , mainContent model
    ]


sideMenu : Model -> Html Msg
sideMenu model =
    aside
        [ class "resources-menu" ]
        [ ul [] (Dict.keys model.schema |> List.sort |> List.map menuItem) ]


menuItem : String -> Html Msg
menuItem name =
    li
        []
        [ a [ href <| "/" ++ name ] [ text <| String.humanize name ] ]


mainContent : Model -> Html Msg
mainContent model =
    case model.route of
        RouteRoot ->
            text ""

        RouteLoadingTable _ _ ->
            loading

        RouteListing listing ->
            Html.map ListingChanged <| Listing.view listing

        RouteDetail listing ->
            Html.map DetailChanged <| Detail.view listing

        RouteFormLoading _ _ ->
            loading

        RouteForm form ->
            Html.map FormChanged <| Form.view form

        RouteNotFound ->
            notFound


notFound : Html Msg
notFound =
    text "Not found"


loading : Html Msg
loading =
    text ""



-- Subscriptions


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.none



-- Routes


getRoute : Url -> Model -> Route
getRoute url model =
    Parser.parse (routeParser url model) url |> Maybe.withDefault RouteNotFound


routeParser : Url -> Model -> Parser (Route -> a) a
routeParser url model =
    Parser.oneOf
        [ Parser.map RouteRoot Parser.top
        , Parser.map (makeListingRoute model url) Parser.string
        , Parser.map
            (\resourcesName ->
                newFormRoute model resourcesName
                    |> RouteLoadingTable resourcesName
            )
            (Parser.string </> s "new")
        , Parser.map
            (\resourcesName id ->
                RouteLoadingTable resourcesName
                    (\table ->
                        RouteDetail
                            (Detail.init
                                { table = table
                                , resourcesName = resourcesName
                                , id = id
                                }
                            )
                    )
            )
            (Parser.string </> Parser.string)
        , Parser.map
            (\resourcesName id ->
                editFormRoute model resourcesName id
                    |> RouteLoadingTable resourcesName
            )
            (Parser.string </> Parser.string </> s "edit")
        ]


makeListingRoute : Model -> Url -> String -> Route
makeListingRoute model url resourcesName =
    let
        modify =
            case model.route of
                RouteListing listing ->
                    if Listing.isSearchVisible listing then
                        Listing.showSearch

                    else
                        Listing.hideSearch

                _ ->
                    Listing.showSearch
    in
    RouteLoadingTable resourcesName
        (Listing.init resourcesName url.query >> modify >> RouteListing)


editFormRoute : Model -> String -> String -> Table -> Route
editFormRoute model resourcesName id table =
    RouteFormLoading (makeForm model resourcesName (Just id) table) id


newFormRoute : Model -> String -> Table -> Route
newFormRoute model resourcesName table =
    RouteForm (makeForm model resourcesName Nothing table)


makeForm : Model -> String -> Maybe String -> Table -> Form
makeForm { formFields } resourcesName id table =
    Form.init
        { resourcesName = resourcesName
        , table = table
        , fieldNames =
            Dict.get resourcesName formFields
                |> Maybe.withDefault []
        , id = id
        }
        table


fieldNames : Model -> String -> List String
fieldNames { formFields } resourcesName =
    Dict.get resourcesName formFields
        |> Maybe.withDefault []
