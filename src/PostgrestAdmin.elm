module PostgrestAdmin exposing (Model, Msg, application, applicationParams)

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
import PostgrestAdmin.Config as Config exposing (Config)
import PostgrestAdmin.OuterMsg as OuterMsg exposing (OuterMsg)
import Route exposing (Route)
import String.Extra as String
import Url exposing (Url)
import Url.Parser as Parser exposing ((</>), Parser)
import Utils.Task exposing (Error(..), attemptWithError, fail)


type Msg
    = SchemaFetched Schema
    | ListingChanged Listing.Msg
    | FormChanged Form.Msg
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
            , host = config.url
            , authScheme = config.authScheme
            , notification = Notification.none
            , decodingError = Nothing
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

        ListingChanged innerMsg ->
            let
                updateFun =
                    Listing.update model innerMsg
                        >> updateRoute Route.Listing ListingChanged model
                        >> handleOuterMsg (Listing.outerMsg innerMsg)
            in
            Route.toListing model.route
                |> Maybe.map updateFun
                |> Maybe.withDefault ( model, Cmd.none )

        FormChanged innerMsg ->
            let
                updateFun =
                    Form.update model innerMsg
                        >> updateRoute Route.Form FormChanged model
                        >> handleOuterMsg (Form.outerMsg innerMsg)
            in
            Route.toForm model.route
                |> Maybe.map updateFun
                |> Maybe.withDefault ( model, Cmd.none )

        NotificationChanged innerMsg ->
            updateNotification innerMsg model

        LinkClicked urlRequest ->
            case urlRequest of
                Browser.Internal url ->
                    ( model, Nav.pushUrl model.key <| Url.toString url )

                Browser.External href ->
                    ( model, Nav.load href )

        UrlChanged url ->
            navigate { model | route = getRoute url model }

        Failed err ->
            failed err model


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
            Listing.fetch model listing
                |> updateRoute Route.Listing ListingChanged model

        Route.FormLoading form id ->
            ( model, Form.fetch model form id |> Cmd.map FormChanged )

        _ ->
            ( model, Cmd.none )


updateRoute :
    (a -> Route)
    -> (innerMsg -> Msg)
    -> Model
    -> ( a, Cmd innerMsg )
    -> ( Model, Cmd Msg )
updateRoute makeRoute makeMsg model ( a, cmd ) =
    ( { model | route = makeRoute a }
    , Cmd.map makeMsg cmd
    )


handleOuterMsg : OuterMsg -> ( Model, Cmd Msg ) -> ( Model, Cmd Msg )
handleOuterMsg msg ( model, cmd ) =
    case msg of
        OuterMsg.RequestFailed err ->
            failed err model

        OuterMsg.NotificationChanged notificationMsg ->
            updateNotification notificationMsg model

        OuterMsg.Pass ->
            ( model, cmd )


failed : Error -> Model -> ( Model, Cmd Msg )
failed err model =
    case err of
        PGError (PG.BadStatus 401 _ { message }) ->
            case message of
                Just "JWT expired" ->
                    ( model, Cmd.none )

                _ ->
                    ( model, Cmd.none )

        _ ->
            ( model, Cmd.none )


updateNotification : Notification.Msg -> Model -> ( Model, Cmd Msg )
updateNotification msg model =
    ( { model | notification = Notification.update msg }, Cmd.none )



-- View


view : Model -> Browser.Document Msg
view model =
    { title = "Admin"
    , body =
        [ div
            [ class "main-container" ]
            [ sideMenu model
            , div [ class "main-area" ] (body model)
            ]
        ]
    }


body : Model -> List (Html Msg)
body model =
    case model.decodingError of
        Just err ->
            [ h1 [] [ text "Init failed" ]
            , pre
                [ class "parse-errors" ]
                [ text (Decode.errorToString err) ]
            ]

        Nothing ->
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
