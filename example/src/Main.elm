port module Main exposing (main)

import Browser
import Browser.Navigation as Nav
import Dict
import Html exposing (Html, div, h1, text)
import Http
import Json.Decode as Decode
import PostgRestAdmin


port loggedIn : String -> Cmd msg


port loggedOut : () -> Cmd msg


type alias Model =
    { adminAppModel : PostgRestAdmin.Model }


type SelectorMsg
    = Noop


type Msg
    = SelectorChanged SelectorMsg
    | AdminAppChanged PostgRestAdmin.Msg


main : Program Decode.Value Model Msg
main =
    let
        params =
            adminAppParams
    in
    Browser.application
        { init = params.init
        , view =
            \model ->
                { title = "PostgREST Admin"
                , body =
                    [ div
                        []
                        [ params.view model ]
                    ]
                }
        , update =
            \msg ->
                case msg of
                    AdminAppChanged innerMsg ->
                        params.update innerMsg

                    SelectorChanged innerMsg ->
                        update innerMsg
        , subscriptions = params.subscriptions
        , onUrlRequest = params.onUrlRequest
        , onUrlChange = params.onUrlChange
        }


adminAppParams : PostgRestAdmin.AppParams Model Msg
adminAppParams =
    PostgRestAdmin.configure
        |> PostgRestAdmin.withHost "http://localhost:9080"
        |> PostgRestAdmin.withClientHeaders
            [ Http.header "Accept-Profile" "bluebox"
            , Http.header "Content-Profile" "bluebox"
            ]
        |> PostgRestAdmin.onLogin loggedIn
        |> PostgRestAdmin.onLogout loggedOut
        |> PostgRestAdmin.buildAppParams
            { toInnerModel = .adminAppModel
            , toOuterModel =
                \app model -> { model | adminAppModel = app }
            , toOuterMsg = AdminAppChanged
            , initModel = \app -> { adminAppModel = app }
            }


update : SelectorMsg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Noop ->
            ( model, Cmd.none )


view : Model -> Browser.Document Msg
view model =
    { title = "Hello World"
    , body =
        [ div []
            [ h1 [] [ text "Hello World" ]
            ]
        ]
    }


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.none
