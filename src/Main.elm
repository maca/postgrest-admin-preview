module Main exposing (main)

import Browser
import Browser.Navigation as Nav
import Dict
import Dict.Extra as Dict
import Form exposing (Form(..))
import Html exposing (Html, a, aside, div, li, text, ul)
import Html.Attributes exposing (class, href)
import Inflect as String
import Listing exposing (Listing)
import Message
import Postgrest.Client as PG
import Postgrest.Resource.Client exposing (Client)
import Postgrest.Schema as Schema exposing (Schema)
import Postgrest.Schema.Definition exposing (Definition)
import Postgrest.Value exposing (Value(..))
import String.Extra as String
import Url exposing (Url)
import Url.Builder as Url
import Url.Parser as Parser exposing ((</>), Parser)
import Utils.Task exposing (Error(..), attemptWithError, fail)


type Msg
    = SchemaFetched Schema
    | ListingChanged Listing Listing.Msg
    | FormChanged Form Form.Msg
    | LinkClicked Browser.UrlRequest
    | UrlChanged Url
    | Failed Error


type Route
    = Root
    | LoadingDefinition String (Definition -> Route)
    | Listing Listing
    | FormLoad Form.Params Form String
    | Form Form
    | NotFound


type alias Model =
    Client
        { route : Route
        , key : Nav.Key
        }


main : Program () Model Msg
main =
    Browser.application
        { init = init
        , update = update
        , view = view
        , subscriptions = subscriptions
        , onUrlRequest = LinkClicked
        , onUrlChange = UrlChanged
        }


init : () -> Url.Url -> Nav.Key -> ( Model, Cmd Msg )
init () url key =
    let
        jwt =
            PG.jwt "eyJhbGciOiJIUzI1NiIsInR5cCI6IkpXVCJ9.eyJyb2xlIjoidG9kb191c2VyIn0.gm7S31FmVXlluCKr2ZBXBolkei2n06gNGJaw1IUJBEk"

        host =
            "http://localhost:3000"

        schema =
            Dict.fromList []

        model =
            { route = Root
            , key = key
            , schema = schema
            , host = host
            , jwt = jwt
            }
    in
    ( { model | route = getRoute url model }
    , Schema.getSchema host |> attemptWithError Failed SchemaFetched
    )



-- Update


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        SchemaFetched schema ->
            urlChanged { model | schema = schema }

        ListingChanged listing lMsg ->
            Listing.update model lMsg listing
                |> mapNested Listing ListingChanged model

        FormChanged form fMsg ->
            Form.update model fMsg form
                |> mapNested Form FormChanged model

        LinkClicked urlRequest ->
            case urlRequest of
                Browser.Internal url ->
                    ( model, Nav.pushUrl model.key <| Url.toString url )

                Browser.External href ->
                    ( model, Nav.load href )

        UrlChanged url ->
            urlChanged { model | route = getRoute url model }

        Failed _ ->
            ( model, Cmd.none )


urlChanged : Model -> ( Model, Cmd Msg )
urlChanged model =
    case model.route of
        LoadingDefinition resourcesName makeRoute ->
            case Dict.get resourcesName model.schema of
                Just definition ->
                    urlChanged { model | route = makeRoute definition }

                Nothing ->
                    ( model, fail Failed <| BadSchema resourcesName )

        Listing listing ->
            Listing.fetch model listing
                |> mapNested Listing ListingChanged model

        FormLoad params form id ->
            ( model, Form.fetch model params id |> Cmd.map (FormChanged form) )

        _ ->
            ( model, Cmd.none )


mapNested :
    (a -> Route)
    -> (a -> innerMsg -> Msg)
    -> Model
    -> ( a, Cmd innerMsg )
    -> ( Model, Cmd Msg )
mapNested makeRoute makeMsg model ( a, cmd ) =
    ( { model | route = makeRoute a }
    , Cmd.map (makeMsg a) cmd
    )



-- View


view : Model -> Browser.Document Msg
view model =
    { title = "Admin"
    , body = body model
    }


body : Model -> List (Html Msg)
body model =
    [ div
        [ class "main-container" ]
        [ sideMenu model
        , div
            [ class "main-area" ]
            [ displayMainContent model ]
        ]
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


displayMainContent : Model -> Html Msg
displayMainContent model =
    case model.route of
        Root ->
            text ""

        LoadingDefinition _ _ ->
            loading

        Listing listing ->
            Html.map (ListingChanged listing) <| Listing.view listing

        FormLoad _ _ _ ->
            loading

        Form form ->
            Html.map (FormChanged form) <| Form.view form

        NotFound ->
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
    Parser.parse (routeParser model) url |> Maybe.withDefault NotFound


routeParser : Model -> Parser (Route -> a) a
routeParser model =
    Parser.oneOf
        [ Parser.map Root Parser.top
        , formRouteParser model
        , Parser.map (\s -> LoadingDefinition s (makeListingRoute s))
            Parser.string
        ]


formRouteParser : Model -> Parser (Route -> a) a
formRouteParser model =
    Parser.map (\res id -> LoadingDefinition res (makeFormRoute res id model))
        (Parser.string </> Parser.string)


makeFormRoute : String -> String -> Model -> Definition -> Route
makeFormRoute resources id model definition =
    let
        message =
            case model.route of
                Form f ->
                    if Form.id f == Just id then
                        Form.message f

                    else
                        Message.none

                _ ->
                    Message.none

        params =
            { resourcesName = resources
            , definition = definition
            , message = message
            }

        form =
            Form.fromDefinition params definition
    in
    if id == "new" then
        Form form

    else
        FormLoad params form id


makeListingRoute : String -> Definition -> Route
makeListingRoute resources definition =
    Listing <| Listing.init resources definition
