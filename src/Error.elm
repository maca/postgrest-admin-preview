module Error exposing (Error(..))

import Http
import Json.Decode as Decode
import Postgrest.Client as PG


type Error
    = HttpError Http.Error
    | DecodeError Decode.Error
    | PGError PG.Error
    | BadSchema String
