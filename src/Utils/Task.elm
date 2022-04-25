module Utils.Task exposing
    ( Error(..)
    , attemptWithError
    , errorToString
    , fail
    , handleJsonResponse
    , toError
    )

import Http
import Json.Decode as Decode exposing (Decoder)
import Postgrest.Client as PG
import Postgrest.Value exposing (ForeignKeyParams)
import Task exposing (Task)


type Error
    = HttpError Http.Error
    | DecodeError Decode.Error
    | PGError PG.Error
    | BadSchema String
    | AutocompleteError ForeignKeyParams String
    | AuthError


attemptWithError : (error -> msg) -> (a -> msg) -> Task error a -> Cmd msg
attemptWithError failure success task =
    let
        tagger result =
            case result of
                Ok a ->
                    success a

                Err err ->
                    failure err
    in
    Task.attempt tagger task


fail : (Error -> msg) -> Error -> Cmd msg
fail tagger err =
    Task.fail err
        |> attemptWithError tagger tagger


handleJsonResponse : Decoder a -> Http.Response String -> Result Error a
handleJsonResponse decoder response =
    case response of
        Http.BadUrl_ url ->
            Err <| HttpError (Http.BadUrl url)

        Http.Timeout_ ->
            Err <| HttpError Http.Timeout

        Http.BadStatus_ { statusCode } _ ->
            Err <| HttpError (Http.BadStatus statusCode)

        Http.NetworkError_ ->
            Err <| HttpError Http.NetworkError

        Http.GoodStatus_ _ body ->
            case Decode.decodeString decoder body of
                Err err ->
                    Err <| DecodeError err

                Ok result ->
                    Ok result


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
        PGError (PG.BadStatus 403 _ _) ->
            "You are not authorized to perform this action"

        HttpError _ ->
            "Something went wrong with the connection, please try again later"

        DecodeError err ->
            Decode.errorToString err

        _ ->
            let
                _ =
                    Debug.log "Err" error
            in
            "Something went wrong, we'll fix soon"
