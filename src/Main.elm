module Main exposing (main)

import Browser exposing (UrlRequest)
import Browser.Navigation as Nav
import Html exposing (..)
import Html.Attributes exposing (class, href)
import Http
import Json.Decode as Decode
import Json.Encode as Encode
import Postgrest.Client as PG
import Result
import Schema exposing (Definition, Schema, Value(..))
import String.Extra as String
import Url exposing (Url)
import Url.Parser as Parser exposing (Parser)


type Msg
    = UpdateSchema (Result Http.Error String)
    | UpdateListing (Result Http.Error String)
    | LinkClicked Browser.UrlRequest
    | UrlChanged Url.Url


type Error
    = HttpError Http.Error
    | DecodeError Decode.Error


type Route
    = Listing String
    | Root
    | NotFound


type alias Model =
    { route : Route
    , key : Nav.Key
    , schema : Maybe Schema
    }


host : String
host =
    "http://localhost:3000"


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
    ( Model (getRoute url) key Nothing, getSchema )



-- Update


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        UpdateSchema result ->
            case decodeSchema result of
                Ok schema ->
                    ( { model | schema = Just schema }, Cmd.none )

                Err _ ->
                    ( model, Cmd.none )

        UpdateListing result ->
            ( model, Cmd.none )

        LinkClicked urlRequest ->
            case urlRequest of
                Browser.Internal url ->
                    ( model, Nav.pushUrl model.key (Url.toString url) )

                Browser.External href ->
                    ( model, Nav.load href )

        UrlChanged url ->
            ( { model | route = getRoute url }, Cmd.none )


decodeSchema : Result Http.Error String -> Result Error Schema
decodeSchema result =
    Result.mapError HttpError result
        |> Result.andThen
            (Decode.decodeString Schema.decoder >> Result.mapError DecodeError)


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
            [ mainContent model ]
        ]
    ]


sideMenu : Model -> Html Msg
sideMenu model =
    aside
        [ class "resources-menu" ]
        [ ul
            []
            (Maybe.map (List.map menuItem) model.schema |> Maybe.withDefault [])
        ]


menuItem : Definition -> Html Msg
menuItem { name } =
    li
        []
        [ a [ href <| "/" ++ name ] [ text <| String.humanize name ] ]


mainContent : Model -> Html Msg
mainContent model =
    case model.route of
        Root ->
            text ""

        Listing name ->
            listing name model

        NotFound ->
            notFound


listing : String -> Model -> Html Msg
listing name { schema } =
    case Maybe.map (List.filter (.name >> (==) name)) schema of
        Just [ { fields } ] ->
            let
                toHeaders =
                    .name >> String.humanize >> text >> List.singleton >> th []

                toValue =
                    .value >> displayValue >> List.singleton >> td []
            in
            table
                []
                [ thead [] [ tr [] (List.map toHeaders fields) ]
                , tbody [] [ tr [] [] ]
                ]

        Nothing ->
            text ""

        _ ->
            notFound


displayValue : Value -> Html Msg
displayValue value =
    case value of
        PFloat (Just float) ->
            text <| String.fromFloat float

        PInt (Just int) ->
            text <| String.fromInt int

        PString (Just string) ->
            text string

        PBool (Just True) ->
            text "true"

        PBool (Just False) ->
            text "false"

        PForeignKeyString ( assoc, _ ) (Just string) ->
            text string

        PForeignKeyInt ( assoc, _ ) (Just int) ->
            text <| String.fromInt int

        PPrimaryKeyString (Just string) ->
            text string

        PPrimaryKeyInt (Just int) ->
            text <| String.fromInt int

        BadValue _ ->
            text "?"

        _ ->
            text "-"


notFound : Html Msg
notFound =
    text "Not found"



-- Subscriptions and Commands


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.none


getSchema : Cmd Msg
getSchema =
    Http.get
        { url = host, expect = Http.expectString UpdateSchema }



-- Url parsing


getRoute : Url -> Route
getRoute url =
    Parser.parse routeParser url |> Maybe.withDefault NotFound


routeParser : Parser (Route -> a) a
routeParser =
    Parser.oneOf
        [ Parser.map Root Parser.top
        , Parser.map Listing Parser.string
        ]
