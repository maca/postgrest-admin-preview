module Utils.Task exposing
    ( Error(..)
    , errorToString
    , handleJsonResponse
    , handleJsonValue
    , handleResponse
    , toError
    )

import Http
import Json.Decode as Decode exposing (Decoder)
import Json.Encode as Encode exposing (Value)


type Error
    = HttpError Http.Error
    | DecodeError Decode.Error
    | BadSchema String
    | AutocompleteError String
    | RequestError String
    | NoError
    | AuthError


handleJsonValue : Http.Response String -> Result Error Value
handleJsonValue =
    handleJsonResponse <|
        Decode.oneOf
            [ Decode.value
            , Decode.succeed Encode.null
            ]


handleJsonResponse : Decoder a -> Http.Response String -> Result Error a
handleJsonResponse decoder =
    handleResponse
        (\body ->
            case Decode.decodeString decoder body of
                Err err ->
                    Err (DecodeError err)

                Ok result ->
                    Ok result
        )


handleResponse : (body -> Result Error a) -> Http.Response body -> Result Error a
handleResponse toResult response =
    case response of
        Http.BadUrl_ url ->
            Err (HttpError (Http.BadUrl url))

        Http.Timeout_ ->
            Err (HttpError Http.Timeout)

        Http.BadStatus_ { statusCode } _ ->
            case statusCode of
                401 ->
                    Err AuthError

                _ ->
                    Err (HttpError (Http.BadStatus statusCode))

        Http.NetworkError_ ->
            Err (HttpError Http.NetworkError)

        Http.GoodStatus_ _ body ->
            toResult body


toError : Result x a -> Maybe x
toError result =
    case result of
        Err err ->
            Just err

        _ ->
            Nothing


errorToString : Error -> String
errorToString error =
    case error of
        HttpError _ ->
            "Something went wrong with the connection, please try again later"

        DecodeError err ->
            Decode.errorToString err

        _ ->
            genericError


genericError : String
genericError =
    "Something went wrong, we'll fix soon"
