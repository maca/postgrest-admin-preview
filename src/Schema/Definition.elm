module Schema.Definition exposing (Column, Definition, primaryKeyName, toRecord)

import Dict exposing (Dict)
import Dict.Extra as Dict
import PrimaryKey exposing (PrimaryKey)
import Value exposing (Value)


type alias Column =
    { required : Bool
    , value : Value
    }


type alias Definition =
    Dict String Column


toRecord : Definition -> Dict String Value
toRecord definition =
    Dict.map (\_ v -> v.value) definition


primaryKeyName : Definition -> Maybe String
primaryKeyName definition =
    Dict.find (\_ v -> Value.isPrimaryKey v.value) definition
        |> Maybe.map Tuple.first
