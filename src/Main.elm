module Main exposing (main)

import Browser
import Html
    exposing
        ( Attribute
        , Html
        , div
        , h1
        , h2
        , text
        , textarea
        )
import Html.Attributes exposing (style)
import Http
import Json.Decode as Decode
import Json.Encode as Encode
import Postgrest.Client as PG
import Result
import Schema exposing (Schema)


type Msg
    = UpdateSchema (Result Http.Error String)


type alias Model =
    { schema : Maybe Schema
    }


host : String
host =
    "http://localhost:3000"


main : Program () Model Msg
main =
    Browser.element
        { init = always ( Model Nothing, getSchema )
        , update = update
        , view = view
        , subscriptions = subscriptions
        }


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        UpdateSchema result ->
            case result of
                Ok body ->
                    case Decode.decodeString Schema.decoder body of
                        Ok schema ->
                            let
                                _ =
                                    Debug.log "schema" schema
                            in
                            ( { model | schema = Just schema }, Cmd.none )

                        Err _ ->
                            ( model, Cmd.none )

                Err _ ->
                    ( model, Cmd.none )


view : Model -> Html Msg
view _ =
    div
        []
        [ text "Hello" ]


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.none


getSchema : Cmd Msg
getSchema =
    Http.get
        { url = host, expect = Http.expectString UpdateSchema }
