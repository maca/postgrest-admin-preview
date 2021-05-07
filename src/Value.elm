module Value exposing
    ( Value(..)
    , encode
    , foreignKeyReference
    , isForeignKey
    , isPrimaryKey
    , toPrimaryKey
    , update
    )

import Iso8601
import Json.Decode as Decode
import Json.Encode as Encode
import PrimaryKey exposing (PrimaryKey(..))
import String.Extra as String
import Time


type Value
    = PFloat (Maybe Float)
    | PInt (Maybe Int)
    | PString (Maybe String)
    | PBool (Maybe Bool)
    | PTime (Maybe Time.Posix)
    | PPrimaryKey (Maybe PrimaryKey)
    | PForeignKey ( String, String ) (Maybe String) (Maybe PrimaryKey)
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

        PTime mposix ->
            enc Encode.string (Maybe.map Iso8601.fromTime mposix)

        PPrimaryKey mpk ->
            enc PrimaryKey.encode mpk

        PForeignKey _ _ mpk ->
            enc PrimaryKey.encode mpk

        BadValue val ->
            Encode.null


update : Value -> String -> Value
update value string =
    case value of
        PString _ ->
            PString <| String.nonBlank string

        PFloat _ ->
            PFloat <| String.toFloat string

        PInt _ ->
            PInt <| String.toInt string

        PBool prev ->
            PBool <| Maybe.map not prev

        PTime _ ->
            let
                string_ =
                    if String.length string == 16 then
                        string ++ ":00"

                    else
                        string
            in
            PTime <| Result.toMaybe <| Iso8601.toTime string_

        other ->
            other


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


foreignKeyReference : Value -> Maybe ( String, String )
foreignKeyReference value =
    case value of
        PForeignKey column _ _ ->
            Just column

        _ ->
            Nothing
