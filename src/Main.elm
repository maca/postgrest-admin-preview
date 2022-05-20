module Main exposing (main)

import BasicAuth
import Html exposing (Html, button, div, text)
import Html.Events exposing (onClick)
import Postgrest.Record as Record exposing (Record)
import PostgrestAdmin
import PostgrestAdmin.Config as Config exposing (tableNameParser)
import Url.Parser as Parser exposing ((</>), s)


type Msg
    = Up
    | Down


type alias Model =
    { count : Int
    , record : Record
    }


view : Model -> Html Msg
view { count, record } =
    div
        []
        [ text (Record.tableName record)
        , text (String.fromInt count)
        , button
            [ onClick Up ]
            [ text "+" ]
        , button
            [ onClick Down ]
            [ text "-" ]
        ]


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Up ->
            ( { model | count = model.count + 1 }, Cmd.none )

        Down ->
            ( { model | count = model.count - 1 }, Cmd.none )


main : PostgrestAdmin.Program Model Msg
main =
    Config.init
        |> Config.withBasicAuth BasicAuth.config
        |> Config.withResourceMountPoint
            { view = \model -> view model
            , update = update
            , init = \record -> ( { count = 1, record = record }, Cmd.none )
            }
            (tableNameParser "workflows" </> Parser.string </> s "form")
        |> PostgrestAdmin.application
