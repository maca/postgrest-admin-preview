module Schema exposing (Field, Schema, Value(..), decoder)

import Basics.Extra exposing (uncurry)
import Dict exposing (Dict)
import Json.Decode as Decode
    exposing
        ( Decoder
        , andThen
        , bool
        , field
        , float
        , int
        , maybe
        , string
        )
import Regex exposing (Regex)


type alias Schema =
    Dict String (Dict String Field)


type alias Field =
    { required : Bool
    , value : Value
    }


type alias Column =
    ( String, String )


type Value
    = PFloat (Maybe Float)
    | PInt (Maybe Int)
    | PString (Maybe String)
    | PBool (Maybe Bool)
    | PForeignKeyString Column (Maybe String)
    | PForeignKeyInt Column (Maybe Int)
    | PPrimaryKeyString (Maybe String)
    | PPrimaryKeyInt (Maybe Int)
    | BadValue String


type Triple a b c
    = Triple a b c


decoder : Decoder Schema
decoder =
    field "definitions" (Decode.dict fieldsDecoder)


fieldsDecoder : Decoder (Dict String Field)
fieldsDecoder =
    field "required" (Decode.list string) |> andThen propertiesDecoder


propertiesDecoder : List String -> Decoder (Dict String Field)
propertiesDecoder required =
    let
        mapField ( name, value ) =
            ( name, Field (List.member name required) value )
    in
    Decode.map (Dict.fromList << List.map mapField)
        (field "properties" (Decode.keyValuePairs valueDecoder))


valueDecoder : Decoder Value
valueDecoder =
    Decode.map3 Triple
        (field "type" string)
        (field "format" string)
        (maybe <| field "description" string)
        |> andThen
            (\data ->
                case data of
                    Triple "number" _ _ ->
                        mapValue PFloat float

                    Triple "integer" _ maybeDesc ->
                        Decode.oneOf
                            [ mapPrimaryKey PPrimaryKeyInt int maybeDesc
                            , mapForeignKey PForeignKeyInt int maybeDesc
                            , mapValue PInt int
                            ]

                    Triple "string" _ maybeDesc ->
                        Decode.oneOf
                            [ mapPrimaryKey PPrimaryKeyString string maybeDesc
                            , mapForeignKey PForeignKeyString string maybeDesc
                            , mapValue PString string
                            ]

                    Triple "boolean" _ _ ->
                        mapValue PBool bool

                    Triple k v _ ->
                        Decode.succeed <|
                            BadValue <|
                                "unknown type ("
                                    ++ k
                                    ++ ", "
                                    ++ v
                                    ++ ")"
            )


mapPrimaryKey : (Maybe a -> Value) -> Decoder a -> Maybe String -> Decoder Value
mapPrimaryKey const dec maybeDesc =
    case Maybe.map (Regex.contains primaryKeyRegex) maybeDesc of
        Just True ->
            mapValue const dec

        _ ->
            Decode.fail ""


mapForeignKey :
    (Column -> Maybe a -> Value)
    -> Decoder a
    -> Maybe String
    -> Decoder Value
mapForeignKey const dec maybeDesc =
    let
        matchFn =
            List.concatMap .submatches << Regex.find foreignKeyRegex
    in
    case Maybe.map matchFn maybeDesc of
        Just [ Just table, Just col ] ->
            mapValue (const ( table, col )) dec

        _ ->
            Decode.fail ""


mapValue : (Maybe a -> Value) -> Decoder a -> Decoder Value
mapValue cons dec =
    Decode.map cons (maybe <| field "default" dec)


primaryKeyRegex : Regex
primaryKeyRegex =
    Regex.fromString
        "Primary Key"
        |> Maybe.withDefault Regex.never


foreignKeyRegex : Regex
foreignKeyRegex =
    Regex.fromString
        "fk table='(\\w+)' column='(\\w+)'"
        |> Maybe.withDefault Regex.never
