module Postgrest.Schema.Table exposing
    ( Column
    , Table
    , columnValue
    , primaryKeyName
    )

import Dict exposing (Dict)
import Dict.Extra as Dict
import Json.Decode exposing (Decoder)
import Postgrest.Value as Value exposing (Value)


type alias Column =
    { required : Bool
    , decoder : Decoder Value
    , value : Value
    }


type alias Table =
    Dict String Column


primaryKeyName : Table -> Maybe String
primaryKeyName table =
    Dict.find (\_ { value } -> Value.isPrimaryKey value) table
        |> Maybe.map Tuple.first


columnValue : Column -> Value
columnValue { value } =
    value
