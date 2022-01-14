module BasicAuth exposing
    ( BasicAuth
    , Msg
    , noFlags
    , toJwt
    , update
    , view
    , withDecoder
    , withEncoder
    , withUrl
    )

import Dict exposing (Dict)
import Html exposing (Html, button, div, fieldset, form, input, label, text)
import Html.Attributes exposing (class, disabled, for, id, type_, value)
import Html.Events exposing (onInput, onSubmit)
import Http
import Json.Decode as Decode exposing (Decoder)
import Json.Encode as Encode exposing (Value)
import Postgrest.Client as PG
import String.Extra as String
import Task exposing (Task)
import Url exposing (Protocol(..), Url)


type Msg
    = InputChanged String String
    | Submitted
    | GotHttp (Result Http.Error PG.JWT)


type alias Params =
    { url : Url
    , decoder : Decoder PG.JWT
    , encoder : Dict String String -> Value
    , fields : List ( String, String )
    }


type BasicAuth
    = BasicAuth Params (Maybe PG.JWT)


noFlags : Decoder BasicAuth
noFlags =
    BasicAuth
        { url =
            { protocol = Http
            , host = "localhost"
            , port_ = Just 4000
            , path = "rpc/login"
            , query = Nothing
            , fragment = Nothing
            }
        , decoder = Decode.map PG.jwt (Decode.field "token" Decode.string)
        , encoder = Encode.dict identity Encode.string
        , fields = [ ( "email", "" ), ( "password", "" ) ]
        }
        Nothing
        |> Decode.succeed


withUrl : String -> Decoder BasicAuth -> Decoder BasicAuth
withUrl urlStr decoder =
    decoder |> Decode.andThen (withUrlHelp urlStr)


withUrlHelp urlStr (BasicAuth params m) =
    Url.fromString urlStr
        |> Maybe.map
            (\url -> Decode.succeed <| BasicAuth { params | url = url } m)
        |> Maybe.withDefault
            (Decode.fail "`BasicAuth.withUrl` was given an invalid URL")


withDecoder : Decoder PG.JWT -> Decoder BasicAuth -> Decoder BasicAuth
withDecoder jwtDecoder decoder =
    decoder
        |> Decode.andThen
            (\(BasicAuth params m) ->
                BasicAuth { params | decoder = jwtDecoder } m
                    |> Decode.succeed
            )


withEncoder :
    (Dict String String -> Value)
    -> Decoder BasicAuth
    -> Decoder BasicAuth
withEncoder encoder decoder =
    decoder
        |> Decode.andThen
            (\(BasicAuth params m) ->
                BasicAuth { params | encoder = encoder } m
                    |> Decode.succeed
            )



-- Update


update : Msg -> BasicAuth -> ( BasicAuth, Cmd Msg )
update msg ((BasicAuth params m) as auth) =
    case msg of
        InputChanged fieldName value ->
            let
                fields =
                    List.map
                        (\( k, v ) ->
                            if k == fieldName then
                                ( k, value )

                            else
                                ( k, v )
                        )
                        params.fields
            in
            ( BasicAuth { params | fields = fields } m, Cmd.none )

        Submitted ->
            ( auth, requestToken auth )

        GotHttp result ->
            case result of
                Ok token ->
                    ( BasicAuth params (Just token), Cmd.none )

                Err err ->
                    ( BasicAuth
                        { params
                            | fields =
                                List.map clearPassword params.fields
                        }
                        m
                    , Cmd.none
                    )


clearPassword : ( String, String ) -> ( String, String )
clearPassword ( k, v ) =
    if k == "password" || k == "pass" then
        ( k, "" )

    else
        ( k, v )


requestToken : BasicAuth -> Cmd Msg
requestToken (BasicAuth params _) =
    Http.post
        { url = Url.toString params.url
        , body = Http.jsonBody (Dict.fromList params.fields |> params.encoder)
        , expect = Http.expectJson GotHttp params.decoder
        }



-- View


view : BasicAuth -> Html Msg
view (BasicAuth { fields } _) =
    div
        [ class "auth-modal" ]
        [ div
            [ class "auth-form" ]
            [ form
                [ class "auth-form"
                , onSubmit Submitted
                ]
                [ fieldset [] (List.map viewField fields)
                , fieldset []
                    [ button
                        [ disabled <|
                            List.any (Tuple.second >> String.isEmpty) fields
                        ]
                        [ text "Login" ]
                    ]
                ]
            ]
        ]


viewField : ( String, String ) -> Html Msg
viewField ( fieldName, fieldValue ) =
    div
        [ class "field" ]
        [ label [ for fieldName ] [ text <| String.humanize fieldName ]
        , input
            [ id fieldName
            , value fieldValue
            , onInput (InputChanged fieldName)
            , type_ (fieldType fieldName)
            ]
            []
        ]


fieldType fieldName =
    case fieldName of
        "email" ->
            "email"

        "password" ->
            "password"

        "pass" ->
            "password"

        "phone" ->
            "tel"

        "telephone" ->
            "tel"

        _ ->
            "input"


toJwt : BasicAuth -> Maybe PG.JWT
toJwt (BasicAuth _ maybeJwt) =
    maybeJwt
