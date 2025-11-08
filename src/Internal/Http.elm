module Internal.Http exposing
    ( Error(..)
    , removeLeadingOrTrailingSlash
    , toError
    )

import Http
import Json.Decode as Decode
import Regex


type Error
    = HttpError Http.Error
    | DecodeError Decode.Error
    | RequestError String
    | ExpectedRecord
    | ExpectedRecordList
    | AuthError


toError : Result x a -> Maybe x
toError result =
    case result of
        Err err ->
            Just err

        _ ->
            Nothing


removeLeadingOrTrailingSlash : String -> String
removeLeadingOrTrailingSlash =
    Regex.replace
        (Maybe.withDefault Regex.never (Regex.fromString "^/|/$"))
        (always "")
