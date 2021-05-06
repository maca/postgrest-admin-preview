module Value exposing
    ( Column
    , Value(..)
    , encode
    , foreignKeyReference
    , isForeignKey
    , isPrimaryKey
    , toPrimaryKey
    )

import Json.Decode as Decode
import Json.Encode as Encode
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


encode : Value -> Encode.Value
encode value =
    let
        enc e =
            Maybe.map e >> Maybe.withDefault Encode.null
    in
    case value of
        PFloat mfloat ->
            enc Encode.float mfloat

        PInt mint ->
            enc Encode.int mint

        PString mstring ->
            enc Encode.string mstring

        PBool mbool ->
            enc Encode.bool mbool

        PPrimaryKey mpk ->
            enc PrimaryKey.encode mpk

        PForeignKey _ _ mpk ->
            enc PrimaryKey.encode mpk

        BadValue val ->
            Encode.null


isPrimaryKey : Value -> Bool
isPrimaryKey value =
    case value of
        PPrimaryKey _ ->
            True

        _ ->
            False


isForeignKey : Value -> Bool
isForeignKey value =
    case value of
        PForeignKey _ _ _ ->
            True

        _ ->
            False


toPrimaryKey : Value -> Maybe PrimaryKey
toPrimaryKey value =
    case value of
        PPrimaryKey mprimaryKey ->
            mprimaryKey

        _ ->
            Nothing


foreignKeyReference : Value -> Maybe Column
foreignKeyReference value =
    case value of
        PForeignKey column _ _ ->
            Just column

        _ ->
            Nothing
