module Internal.FormAuth exposing
    ( FormAuth
    , Msg
    , Session(..)
    , config
    , fail
    , isFailed
    , isSuccessMsg
    , toJwt
    , update
    , view
    , withAuthUrl
    , withDecoder
    , withEncoder
    )

import Dict exposing (Dict)
import Html exposing (Html, button, div, fieldset, form, input, label, pre, text)
import Html.Attributes exposing (class, disabled, for, id, style, type_, value)
import Html.Events exposing (onInput, onSubmit)
import Http
import Internal.Flag as Flag
import Json.Decode as Decode exposing (Decoder)
import Json.Encode as Encode exposing (Value)
import Postgrest.Client as PG
import String.Extra as String
import Task exposing (Task)
import Url exposing (Protocol(..), Url)
import Utils.Task exposing (attemptWithError)


type Session
    = Token PG.JWT


type Msg
    = InputChanged String String
    | Submitted
    | Succeeded Session
    | Failed Error


type alias Params =
    { url : Url
    , decoder : Decoder Session
    , encoder : Dict String String -> Value
    , fields : List ( String, String )
    }


type Error
    = Forbidden
    | Unauthorized
    | ServerError Int
    | DecodeError Decode.Error
    | NetworkError


type FormAuth
    = Ready Params
    | Active Params
    | Success Params Session
    | Failure Params Error


config : Decoder FormAuth
config =
    let
        params =
            { url =
                { protocol = Http
                , host = "localhost"
                , port_ = Just 3000
                , path = "rpc/login"
                , query = Nothing
                , fragment = Nothing
                }
            , decoder =
                Decode.map (PG.jwt >> Token)
                    (Decode.field "token" Decode.string)
            , encoder = Encode.dict identity Encode.string
            , fields = [ ( "email", "" ), ( "password", "" ) ]
            }
    in
    Decode.oneOf
        [ Decode.field "jwt" Decode.string
            |> Decode.map (\token -> Success params (Token <| PG.jwt token))
        , readyDecoder params
        ]
        |> Flag.string "authUrl" withAuthUrlDecoder


isSuccessMsg : Msg -> Bool
isSuccessMsg msg =
    case msg of
        Succeeded _ ->
            True

        _ ->
            False


fail : FormAuth -> FormAuth
fail auth =
    case auth of
        Ready params ->
            Failure params Unauthorized

        Active params ->
            Failure params Unauthorized

        Success params _ ->
            Failure params Unauthorized

        Failure params _ ->
            Failure params Unauthorized



-- UPDATE


update : Msg -> FormAuth -> ( FormAuth, Cmd Msg )
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
            ( Active { params | fields = fields }, Cmd.none )

        Submitted ->
            ( auth
            , requestToken auth
                |> attemptWithError Failed Succeeded
            )

        Succeeded session ->
            ( Success { params | fields = clearPassword params.fields } session
            , Cmd.none
            )

        Failed error ->
            ( Failure { params | fields = clearPassword params.fields } error
            , Cmd.none
            )


clearPassword : List ( String, String ) -> List ( String, String )
clearPassword fields =
    List.map clearPasswordHelp fields


clearPasswordHelp : ( String, String ) -> ( String, String )
clearPasswordHelp ( k, v ) =
    if k == "password" || k == "pass" then
        ( k, "" )

    else
        ( k, v )


requestToken : FormAuth -> Task Error Session
requestToken auth =
    let
        params =
            toParams auth
    in
    Http.task
        { method = "POST"
        , headers = []
        , url = Url.toString params.url
        , body = Http.jsonBody (Dict.fromList params.fields |> params.encoder)
        , resolver = Http.stringResolver <| handleJsonResponse <| params.decoder
        , timeout = Nothing
        }


handleJsonResponse : Decoder a -> Http.Response String -> Result Error a
handleJsonResponse decoder response =
    case response of
        Http.BadStatus_ { statusCode } _ ->
            if statusCode == 401 || statusCode == 403 then
                Err <| Forbidden

            else
                Err <| ServerError statusCode

        Http.GoodStatus_ _ body ->
            case Decode.decodeString decoder body of
                Err err ->
                    Err <| DecodeError err

                Ok result ->
                    Ok result

        _ ->
            Err <| NetworkError



-- VIEW


view : FormAuth -> Html Msg
view auth =
    if requiresAuthentication auth then
        viewForm auth

    else
        text ""


viewForm : FormAuth -> Html Msg
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


toJwt : FormAuth -> Maybe PG.JWT
toJwt auth =
    case auth of
        Ready _ ->
            Nothing

        Active _ ->
            Nothing

        Success _ token ->
            Just (sessionToJwt token)

        Failure _ _ ->
            Nothing


isFailed : FormAuth -> Bool
isFailed auth =
    case auth of
        Failure _ _ ->
            True

        _ ->
            False


sessionToJwt : Session -> PG.JWT
sessionToJwt session =
    case session of
        Token token ->
            token


requiresAuthentication : FormAuth -> Bool
requiresAuthentication auth =
    case auth of
        Ready _ ->
            True

        Active _ ->
            True

        Success _ _ ->
            False

        Failure _ _ ->
            True


toParams : FormAuth -> Params
toParams auth =
    case auth of
        Ready params ->
            params

        Active params ->
            params

        Success params _ ->
            params

        Failure params _ ->
            params


errorMessage : FormAuth -> Html Msg
errorMessage auth =
    case auth of
        Failure _ error ->
            case error of
                Forbidden ->
                    errorWrapper
                        [ text """You may have entered the wrong password,
                          please try again."""
                        ]

                Unauthorized ->
                    errorWrapper
                        [ text
                            "Please sign in to continue."
                        ]

                ServerError status ->
                    errorWrapper
                        [ text "The server responded with an error: "
                        , pre [] [ text (String.fromInt status) ]
                        ]

                DecodeError err ->
                    errorWrapper
                        [ text "There was an issue parsing the server response: "
                        , pre [] [ text (Decode.errorToString err) ]
                        ]

                NetworkError ->
                    errorWrapper
                        [ text """There was an issue reaching the server,
                          please try again later."""
                        ]

        _ ->
            div [ class "form-error-message", style "visibility" "hidden" ] []


errorWrapper : List (Html Msg) -> Html Msg
errorWrapper html =
    div [ class "form-error-message" ] html



-- DECODERS


withAuthUrl : String -> Decoder FormAuth -> Decoder FormAuth
withAuthUrl urlStr decoder =
    decoder |> Decode.andThen (withAuthUrlDecoder urlStr)


withAuthUrlDecoder : String -> FormAuth -> Decoder FormAuth
withAuthUrlDecoder urlStr auth =
    let
        params =
            toParams auth
    in
    Url.fromString urlStr
        |> Maybe.map
            (\url -> updateParams { params | url = url } auth |> Decode.succeed)
        |> Maybe.withDefault
            (Decode.fail "`FormAuth.withAuthUrl` was given an invalid URL")


withEncoder :
    (Dict String String -> Value)
    -> Decoder FormAuth
    -> Decoder FormAuth
withEncoder encoder decoder =
    decoder
        |> Decode.andThen
            (\auth ->
                let
                    params =
                        toParams auth
                in
                updateParams { params | encoder = encoder } auth
                    |> Decode.succeed
            )


withDecoder : Decoder String -> Decoder FormAuth -> Decoder FormAuth
withDecoder jwtDecoder decoder =
    decoder
        |> Decode.andThen
            (\auth ->
                let
                    params =
                        toParams auth
                in
                updateParams
                    { params
                        | decoder = Decode.map (PG.jwt >> Token) jwtDecoder
                    }
                    auth
                    |> Decode.succeed
            )


updateParams : Params -> FormAuth -> FormAuth
updateParams params auth =
    case auth of
        Ready _ ->
            Ready params

        Active _ ->
            Active params

        Success _ session ->
            Success params session

        Failure _ error ->
            Failure params error


readyDecoder : Params -> Decoder FormAuth
readyDecoder params =
    Decode.succeed (Ready params)
