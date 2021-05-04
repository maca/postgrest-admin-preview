module Main exposing (main)

import Browser exposing (UrlRequest)
import Browser.Navigation as Nav
import Html
    exposing
        ( Attribute
        , Html
        , a
        , div
        , h1
        , h2
        , li
        , section
        , text
        , textarea
        , ul
        )
import Html.Attributes exposing (href, style)
import Http
import Json.Decode as Decode
import Json.Encode as Encode
import Postgrest.Client as PG
import Result
import Schema exposing (Definition(..), Schema)
import String.Extra as String
import Url exposing (Url)


type Msg
    = UpdateSchema (Result Http.Error String)
    | LinkClicked Browser.UrlRequest
    | UrlChanged Url.Url
    | NoOp


type Error
    = HttpError Http.Error
    | DecodeError Decode.Error


type alias Model =
    { url : Url.Url
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
    ( Model url key Nothing, getSchema )



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

        LinkClicked urlRequest ->
            case urlRequest of
                Browser.Internal url ->
                    ( model, Nav.pushUrl model.key (Url.toString url) )

                Browser.External href ->
                    ( model, Nav.load href )

        UrlChanged url ->
            ( { model | url = url }, Cmd.none )

        NoOp ->
            ( model, Cmd.none )


decodeSchema : Result Http.Error String -> Result Error Schema
decodeSchema result =
    Result.mapError HttpError result
        |> Result.andThen
            (Decode.decodeString Schema.decoder >> Result.mapError DecodeError)



-- View


view : Model -> Browser.Document Msg
view model =
    { title = "Admin"
    , body = body model
    }


body : Model -> List (Html Msg)
body model =
    [ sideMenu model ]


sideMenu : Model -> Html Msg
sideMenu model =
    let
        items =
            Maybe.map (List.map menuItem) model.schema
                |> Maybe.withDefault []
    in
    section
        []
        [ ul [] items ]


menuItem (Definition name _) =
    li
        []
        [ a [ href <| "/" ++ name ] [ text <| String.humanize name ] ]



-- Subscriptions and Commands


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.none


getSchema : Cmd Msg
getSchema =
    Http.get
        { url = host, expect = Http.expectString UpdateSchema }
