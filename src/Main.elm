module Main exposing (main)

import Html exposing (Html, button, div, text)
import Html.Events exposing (onClick)
import PostgrestAdmin
import PostgrestAdmin.BasicAuth as BasicAuth
import PostgrestAdmin.Client exposing (Client)
import PostgrestAdmin.Cmd as AppCmd
import PostgrestAdmin.Config as Config
import Url.Parser as Parser exposing ((</>), s)



-- port loginSuccess : String -> Cmd msg


type Msg
    = LoggedIn Client
    | Up
    | Down


type alias Model =
    { count : Int
    , client : Client
    }


view : Model -> Html Msg
view _ =
    div
        []
        [ button
            [ onClick Up ]
            [ text "+" ]
        , button
            [ onClick Down ]
            [ text "-" ]
        ]


init : Client -> ( Model, AppCmd.Cmd Msg )
init client =
    ( { count = 1, client = client }
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


main : PostgrestAdmin.Program Model Msg
main =
    Config.init
        |> Config.withBasicAuth BasicAuth.config
        |> Config.withMountPoint
            { init = init
            , update = update
            , view = view
            , onLogin = LoggedIn
            }
            (Parser.map (always ())
                (s "workflows" </> Parser.string </> s "forms")
            )
        |> PostgrestAdmin.application
