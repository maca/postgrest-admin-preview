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
import Html exposing (Html, button, div, fieldset, form, input, label, pre, text)
import Html.Attributes exposing (class, disabled, for, id, style, type_, value)
import Html.Events exposing (onInput, onSubmit)
import Http exposing (Error(..))
import Json.Decode as Decode exposing (Decoder)
import Json.Encode as Encode exposing (Value)
import Postgrest.Client as PG
import String.Extra as String
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


type Error
    = Forbidden
    | ServerError Int
    | DecodeError String
    | NetworkError


type BasicAuth
    = Init Params
    | Successful Params PG.JWT
    | Failed Params Error


noFlags : Decoder BasicAuth
noFlags =
    Init
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
        |> Decode.succeed


withUrl : String -> Decoder BasicAuth -> Decoder BasicAuth
withUrl urlStr decoder =
    decoder |> Decode.andThen (withUrlHelp urlStr)


withUrlHelp : String -> BasicAuth -> Decoder BasicAuth
withUrlHelp urlStr auth =
    let
        params =
            toParams auth
    in
    Url.fromString urlStr
        |> Maybe.map (\url -> Decode.succeed <| Init { params | url = url })
        |> Maybe.withDefault
            (Decode.fail "`BasicAuth.withUrl` was given an invalid URL")


withDecoder : Decoder PG.JWT -> Decoder BasicAuth -> Decoder BasicAuth
withDecoder jwtDecoder decoder =
    decoder
        |> Decode.andThen
            (\auth ->
                let
                    params =
                        toParams auth
                in
                Init { params | decoder = jwtDecoder } |> Decode.succeed
            )


withEncoder :
    (Dict String String -> Value)
    -> Decoder BasicAuth
    -> Decoder BasicAuth
withEncoder encoder decoder =
    decoder
        |> Decode.andThen
            (\auth ->
                let
                    params =
                        toParams auth
                in
                Init { params | encoder = encoder } |> Decode.succeed
            )



-- Update


update : Msg -> BasicAuth -> ( BasicAuth, Cmd Msg )
update msg auth =
    let
        params =
            toParams auth
    in
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
            ( Init { params | fields = fields }, Cmd.none )

        Submitted ->
            ( auth, requestToken auth )

        GotHttp result ->
            let
                fields =
                    List.map clearPassword params.fields
            in
            case result of
                Ok token ->
                    ( Successful { params | fields = fields } token
                    , Cmd.none
                    )

                Err error ->
                    ( Failed { params | fields = fields } (mapError error)
                    , Cmd.none
                    )


clearPassword : ( String, String ) -> ( String, String )
clearPassword ( k, v ) =
    if k == "password" || k == "pass" then
        ( k, "" )

    else
        ( k, v )


mapError : Http.Error -> Error
mapError error =
    case error of
        BadStatus 401 ->
            Forbidden

        BadStatus 403 ->
            Forbidden

        BadStatus status ->
            ServerError status

        BadBody string ->
            DecodeError string

        _ ->
            NetworkError


requestToken : BasicAuth -> Cmd Msg
requestToken auth =
    let
        params =
            toParams auth
    in
    Http.post
        { url = Url.toString params.url
        , body = Http.jsonBody (Dict.fromList params.fields |> params.encoder)
        , expect = Http.expectJson GotHttp params.decoder
        }



-- View


view : BasicAuth -> Html Msg
view auth =
    if hasToken auth then
        text ""

    else
        viewForm auth


viewForm : BasicAuth -> Html Msg
viewForm auth =
    let
        { fields } =
            toParams auth
    in
    div
        [ class "auth-modal" ]
        [ div
            [ class "auth-form" ]
            [ errorMessage auth
            , form
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


fieldType : String -> String
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
toJwt auth =
    case auth of
        Init _ ->
            Nothing

        Successful _ token ->
            Just token

        Failed _ _ ->
            Nothing


hasToken : BasicAuth -> Bool
hasToken auth =
    toJwt auth
        |> Maybe.map (always True)
        |> Maybe.withDefault False


toParams : BasicAuth -> Params
toParams auth =
    case auth of
        Init params ->
            params

        Successful params _ ->
            params

        Failed params _ ->
            params


errorMessage : BasicAuth -> Html Msg
errorMessage auth =
    case auth of
        Failed _ error ->
            case error of
                Forbidden ->
                    errorWrapper
                        [ text
                            "It looks like you have provided the wrong password"
                        ]

                ServerError status ->
                    errorWrapper
                        [ text "The server responded with an error:"
                        , pre [] [ text (String.fromInt status) ]
                        ]

                DecodeError message ->
                    errorWrapper
                        [ text "There was an issue parsing the server response:"
                        , pre [] [ text message ]
                        ]

                NetworkError ->
                    errorWrapper
                        [ text """There was an issue reaching the server,
                          please try later"""
                        ]

        _ ->
            div [ class "form-error-message", style "visibility" "hidden" ] []


errorWrapper : List (Html Msg) -> Html Msg
errorWrapper html =
    div [ class "form-error-message" ] html
