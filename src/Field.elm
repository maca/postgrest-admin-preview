module Field exposing (Field, update)

import Value exposing (Value)


type alias Field =
    { error : Maybe String
    , required : Bool
    , value : Value
    }


update : Field -> String -> Field
update field string =
    { field | value = Value.update string field.value }
