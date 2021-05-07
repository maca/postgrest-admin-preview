module Schema.Definition exposing
    ( Column(..)
    , Definition
    , primaryKeyName
    , toRecord
    )

import Dict exposing (Dict)
import Dict.Extra as Dict
import Field exposing (Field)
import Value exposing (Value)


type Column
    = Column Bool Value


type alias Definition =
    Dict String Column


toRecord : Definition -> Dict String Field
toRecord definition =
    Dict.map (\_ (Column _ value) -> Field Nothing False value) definition


primaryKeyName : Definition -> Maybe String
primaryKeyName definition =
    Dict.find (\_ (Column _ value) -> Value.isPrimaryKey value) definition
        |> Maybe.map Tuple.first
