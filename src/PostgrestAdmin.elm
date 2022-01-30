port module PostgrestAdmin exposing (Model, Msg, application, applicationParams)

import Browser
import Browser.Navigation as Nav
import Dict
import Dict.Extra as Dict
import Form exposing (Form(..))
import Html exposing (Html, a, aside, div, h1, li, pre, text, ul)
import Html.Attributes exposing (class, href)
import Inflect as String
import Json.Decode as Decode exposing (Decoder, Value)
import Listing exposing (Listing)
import Notification exposing (Notification)
import Postgrest.Client as PG
import Postgrest.Resource.Client exposing (Client)
import Postgrest.Schema as Schema exposing (Schema)
import Postgrest.Schema.Definition exposing (Definition)
import PostgrestAdmin.AuthScheme as AuthScheme
import PostgrestAdmin.Config as Config exposing (Config)
import PostgrestAdmin.OuterMsg as OuterMsg exposing (OuterMsg)
import Route exposing (Route)
import String.Extra as String
import Url exposing (Url)
import Url.Parser as Parser exposing ((</>), Parser)
import Utils.Task exposing (Error(..), attemptWithError, fail)


port loginSuccess : String -> Cmd msg


type Msg
    = SchemaFetched Schema
    | ListingChanged Listing.Msg
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
        , decodingError : Maybe Decode.Error
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
            { route = Route.Root
            , key = key
            , schema = Dict.fromList []
            , notification = Notification.none
            , decodingError = Nothing
            , host = config.url
            , authScheme = config.authScheme
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
            ( { model | decodingError = Just error }, Cmd.none )



-- Update


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        SchemaFetched schema ->
            navigate { model | schema = schema }

        ListingChanged childMsg ->
            let
                updateFun =
                    Listing.update model childMsg
                        >> updateRoute Route.Listing ListingChanged model
                        >> handleChildMsg (Listing.mapMsg childMsg)
            in
            Route.toListing model.route
                |> Maybe.map updateFun
                |> Maybe.withDefault ( model, Cmd.none )

        FormChanged childMsg ->
            let
                updateFun =
                    Form.update model childMsg
                        >> updateRoute Route.Form FormChanged model
                        >> handleChildMsg (Form.mapMsg childMsg)
            in
            Route.toForm model.route
                |> Maybe.map updateFun
                |> Maybe.withDefault ( model, Cmd.none )

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
            navigate { model | route = getRoute url model }

        Failed err ->
            ( failed err model, Cmd.none )


navigate : Model -> ( Model, Cmd Msg )
navigate model =
    case model.route of
        Route.LoadingDefinition resourcesName makeRoute ->
            case Dict.get resourcesName model.schema of
                Just definition ->
                    navigate { model | route = makeRoute definition }

                Nothing ->
                    ( model, fail Failed <| BadSchema resourcesName )

        Route.Listing listing ->
            ( listing, Listing.fetch model listing )
                |> updateRoute Route.Listing ListingChanged model

        Route.FormLoading form id ->
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
        Route.Listing listing ->
            Listing.fetch model listing |> Cmd.map ListingChanged

        Route.FormLoading form id ->
            Form.fetch model form id |> Cmd.map FormChanged

        Route.Form form ->
            case Form.id form of
                Just id ->
                    Form.fetch model form id |> Cmd.map FormChanged

                Nothing ->
                    Cmd.none

        _ ->
            Cmd.none


failed : Error -> Model -> Model
failed err ({ authScheme } as model) =
    case err of
        PGError (PG.BadStatus 401 _ _) ->
            { model | authScheme = AuthScheme.fail authScheme }

        AuthError ->
            { model | authScheme = AuthScheme.fail authScheme }

        _ ->
            model



-- View


view : Model -> Browser.Document Msg
view model =
    { title = "Admin"
    , body =
        case model.decodingError of
            Just err ->
                [ h1 [] [ text "Init failed" ]
                , pre
                    [ class "parse-errors" ]
                    [ text (Decode.errorToString err) ]
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
        Route.Root ->
            text ""

        Route.LoadingDefinition _ _ ->
            loading

        Route.Listing listing ->
            Html.map ListingChanged <| Listing.view listing

        Route.FormLoading _ _ ->
            loading

        Route.Form form ->
            Html.map FormChanged <| Form.view form

        Route.NotFound ->
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
    Parser.parse (routeParser url model) url |> Maybe.withDefault Route.NotFound


routeParser : Url -> Model -> Parser (Route -> a) a
routeParser url model =
    Parser.oneOf
        [ Parser.map Route.Root Parser.top
        , Parser.map (makeListingRoute model url) Parser.string
        , formRouteParser
        ]


makeListingRoute : Model -> Url -> String -> Route
makeListingRoute model url resourcesName =
    let
        modify =
            case model.route of
                Route.Listing listing ->
                    if Listing.isSearchVisible listing then
                        Listing.showSearch

                    else
                        Listing.hideSearch

                _ ->
                    Listing.showSearch
    in
    Route.LoadingDefinition resourcesName
        (Listing.init resourcesName url.query >> modify >> Route.Listing)


formRouteParser : Parser (Route -> a) a
formRouteParser =
    Parser.map
        (\res id -> Route.LoadingDefinition res (makeFormRoute res id))
        (Parser.string </> Parser.string)


makeFormRoute : String -> String -> Definition -> Route
makeFormRoute resources id definition =
    let
        params =
            { resourcesName = resources
            , definition = definition
            }

        form =
            Form.fromDefinition params definition
    in
    if id == "new" then
        Route.Form form

    else
        Route.FormLoading form id
