module Main exposing (main)

import Html exposing (Html, button, div, text)
import Html.Events exposing (onClick)
import PostgrestAdmin
import PostgrestAdmin.Client exposing (Client)
import PostgrestAdmin.Cmd as AppCmd
import PostgrestAdmin.Config as Config
import PostgrestAdmin.Config.FormAuth as FormAuth
import Url.Parser as Parser exposing ((</>), s)


type Msg
    = LoggedIn Client
    | Up
    | Down
    | GotId String


type alias Model =
    { count : Int
    , client : Client
    , id : String
    }


view : Model -> Html Msg
view { count, id } =
    div
        []
        [ text id
        , text " - "
        , text (String.fromInt count)
        , button
            [ onClick Up ]
            [ text "+" ]
        , button
            [ onClick Down ]
            [ text "-" ]
        ]


init : Client -> ( Model, AppCmd.Cmd Msg )
init client =
    ( { count = 1, client = client, id = "" }
    , AppCmd.wrap Cmd.none
    )


update : Msg -> Model -> ( Model, AppCmd.Cmd Msg )
update msg model =
    case msg of
        LoggedIn client ->
            ( { model | client = client }
            , AppCmd.none
            )

        Up ->
            ( { model | count = model.count + 1 }
            , AppCmd.none
            )

        Down ->
            ( { model | count = model.count - 1 }
            , AppCmd.none
            )

        GotId string ->
            ( { model | id = string }, AppCmd.none )


main : PostgrestAdmin.Program Model Msg
main =
    Config.init
        |> Config.withFormAuth FormAuth.config
        |> Config.withMountPoint
            { init = init
            , update = update
            , view = view
            , onLogin = LoggedIn
            }
            (Parser.map GotId
                (s "workflows" </> Parser.string </> s "forms")
            )
        |> PostgrestAdmin.application
