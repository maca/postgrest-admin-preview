module Schema.Definition exposing (Definition, Field, primaryKeyName)

import Dict exposing (Dict)
import Dict.Extra as Dict
import PrimaryKey exposing (PrimaryKey)
import Value exposing (Value)


type alias Field =
    { required : Bool
    , value : Value
    }


type alias Definition =
    Dict String Field


primaryKeyName : Definition -> Maybe String
primaryKeyName definition =
    Dict.find (\_ v -> Value.isPrimaryKey v.value) definition
        |> Maybe.map Tuple.first
