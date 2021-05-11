module Postgrest.Field exposing (Field, setError, update, validate)

import Postgrest.Client as PG
import Postgrest.Value as Value exposing (Value(..))


type alias Field =
    { error : Maybe String
    , required : Bool
    , changed : Bool
    , value : Value
    }


update : String -> Field -> Field
update string field =
    validate
        { field
            | value = Value.update string field.value
            , changed = True
        }


validate : Field -> Field
validate field =
    if
        field.required
            && Value.isNothing field.value
            && (not <| Value.isPrimaryKey field.value)
    then
        { field | error = Just "This field is required" }

    else
        { field | error = Nothing }


setError : PG.PostgrestErrorJSON -> Field -> Field
setError { code } field =
    case code of
        Just "23502" ->
            { field | error = Just "This field is required" }

        _ ->
            field
