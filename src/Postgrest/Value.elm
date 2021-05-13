module Postgrest.Value exposing
    ( ForeignKeyParams
    , Value(..)
    , encode
    , foreignKeyReference
    , isForeignKey
    , isNothing
    , isPrimaryKey
    , isTrue
    , toPrimaryKey
    , toString
    , update
    )

import Iso8601
import Json.Decode as Decode
import Json.Encode as Encode
import Postgrest.PrimaryKey as PrimaryKey exposing (PrimaryKey(..))
import String.Extra as String
import Time


type alias ForeignKeyParams =
    { table : String
    , primaryKeyName : String
    , label : Maybe String
    }


type Value
    = PFloat (Maybe Float)
    | PInt (Maybe Int)
    | PString (Maybe String)
    | PBool (Maybe Bool)
    | PTime (Maybe Time.Posix)
    | PPrimaryKey (Maybe PrimaryKey)
    | PForeignKey (Maybe PrimaryKey) ForeignKeyParams
    | BadValue Decode.Value


encode : Value -> Encode.Value
encode value =
    let
        enc e =
            Maybe.map e >> Maybe.withDefault Encode.null
    in
    case value of
        PString mstring ->
            enc Encode.string mstring

        PFloat mfloat ->
            enc Encode.float mfloat

        PInt mint ->
            enc Encode.int mint

        PBool mbool ->
            enc Encode.bool mbool

        PTime mtime ->
            enc Encode.string (Maybe.map Iso8601.fromTime mtime)

        PPrimaryKey mprimaryKey ->
            enc PrimaryKey.encode mprimaryKey

        PForeignKey mprimaryKey _ ->
            enc PrimaryKey.encode mprimaryKey

        BadValue _ ->
            Encode.null


isNothing : Value -> Bool
isNothing value =
    case value of
        PString (Just _) ->
            False

        PFloat (Just _) ->
            False

        PInt (Just _) ->
            False

        PBool (Just _) ->
            False

        PTime (Just _) ->
            False

        PPrimaryKey (Just _) ->
            False

        PForeignKey (Just _) _ ->
            False

        _ ->
            True


update : String -> Value -> Value
update string value =
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
        PForeignKey _ _ ->
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


isTrue : Value -> Maybe Bool
isTrue value =
    case value of
        PBool mbool ->
            mbool

        _ ->
            Nothing


toString : Value -> Maybe String
toString value =
    case value of
        PString mstring ->
            mstring

        PFloat mfloat ->
            Maybe.map String.fromFloat mfloat

        PInt mint ->
            Maybe.map String.fromInt mint

        PBool (Just True) ->
            Just "true"

        PBool (Just False) ->
            Just "false"

        PTime mtime ->
            Maybe.map (Iso8601.fromTime >> String.slice 0 19) mtime

        PPrimaryKey mprimaryKey ->
            Maybe.map PrimaryKey.toString mprimaryKey

        PForeignKey mprimaryKey _ ->
            Maybe.map PrimaryKey.toString mprimaryKey

        _ ->
            Nothing


foreignKeyReference : Value -> Maybe ForeignKeyParams
foreignKeyReference value =
    case value of
        PForeignKey _ params ->
            Just params

        _ ->
            Nothing
