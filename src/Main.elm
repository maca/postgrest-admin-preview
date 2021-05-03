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


type Error
    = HttpError Http.Error
    | DecodeError Decode.Error


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


decodeSchema : Result Http.Error String -> Result Error Schema
decodeSchema result =
    Result.mapError HttpError result
        |> Result.andThen
            (Decode.decodeString Schema.decoder >> Result.mapError DecodeError)



-- View


view : Model -> Html Msg
view _ =
    div
        []
        [ text "Hello" ]



-- Subscriptions and Commands


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.none


getSchema : Cmd Msg
getSchema =
    Http.get
        { url = host, expect = Http.expectString UpdateSchema }
