module Postgrest.Field exposing (Field, setError, update, validate)

import Postgrest.Client as PG
import Postgrest.Constraint as Constraint exposing (Constraint)
import Postgrest.Value as Value exposing (Value(..))


type alias Field =
    { constraint : Constraint
    , error : Maybe String
    , required : Bool
    , changed : Bool
    , value : Value
    }


update : Value -> Field -> Field
update value field =
    validate { field | value = value, changed = True }


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
