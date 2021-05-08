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
    Dict.map (\_ col -> columnToField col) definition


columnToField : Column -> Field
columnToField (Column required value) =
    { error = Nothing
    , required = required
    , changed = False
    , value = value
    }


primaryKeyName : Definition -> Maybe String
primaryKeyName definition =
    Dict.find (\_ (Column _ value) -> Value.isPrimaryKey value) definition
        |> Maybe.map Tuple.first
