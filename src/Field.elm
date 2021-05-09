module Field exposing (Field, update, validate)

import Value exposing (Value(..))


type alias Field =
    { error : Maybe String
    , required : Bool
    , changed : Bool
    , value : Value
    }


update : Field -> String -> Field
update field string =
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
