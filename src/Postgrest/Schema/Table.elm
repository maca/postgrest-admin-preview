module Postgrest.Schema.Table exposing
    ( Column
    , Table
    , columnValue
    , primaryKeyName
    , toResource
    )

import Dict exposing (Dict)
import Dict.Extra as Dict
import Json.Decode exposing (Decoder)
import Postgrest.Field exposing (Field)
import Postgrest.Value as Value exposing (Value)


type alias Column =
    { required : Bool
    , decoder : Decoder Value
    , value : Value
    }


type alias Table =
    Dict String Column


toResource : Table -> Dict String Field
toResource table =
    Dict.map (\_ col -> columnToField col) table


columnToField : Column -> Field
columnToField { required, value } =
    { error = Nothing
    , required = required
    , changed = False
    , value = value
    }


primaryKeyName : Table -> Maybe String
primaryKeyName table =
    Dict.find (\_ { value } -> Value.isPrimaryKey value) table
        |> Maybe.map Tuple.first


columnValue : Column -> Value
columnValue { value } =
    value
