module Error exposing (Error(..), fail)

import Http
import Json.Decode as Decode
import Postgrest.Client as PG
import Postgrest.Value exposing (ForeignKeyParams)
import Task


type Error
    = HttpError Http.Error
    | DecodeError Decode.Error
    | PGError PG.Error
    | AutocompleteError ForeignKeyParams String
    | BadSchema String


fail : (Result Error Never -> msg) -> Error -> Cmd msg
fail tagger err =
    Task.fail err |> Task.attempt tagger
