module Value exposing (Column, Value(..), foreignKeyReference)

import Json.Decode as Decode
import PrimaryKey exposing (PrimaryKey(..))


type alias Column =
    ( String, String )


type Value
    = PFloat (Maybe Float)
    | PInt (Maybe Int)
    | PString (Maybe String)
    | PBool (Maybe Bool)
    | PPrimaryKey (Maybe PrimaryKey)
    | PForeignKey Column (Maybe String) (Maybe PrimaryKey)
    | BadValue Decode.Value


foreignKeyReference : Value -> Maybe Column
foreignKeyReference value =
    case value of
        PForeignKey column _ _ ->
            Just column

        _ ->
            Nothing
