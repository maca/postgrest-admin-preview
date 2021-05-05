module Schema exposing (Definition, Field, Schema, decoder)

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
import PrimaryKey exposing (PrimaryKey(..))
import Regex exposing (Regex)
import Value exposing (Column, Value(..))


type alias Schema =
    Dict String Definition


type alias Definition =
    Dict String Field


type alias Field =
    { required : Bool
    , value : Value
    }


type Triple a b c
    = Triple a b c


decoder : Decoder Schema
decoder =
    field "definitions" (Decode.dict fieldsDecoder)


fieldsDecoder : Decoder Definition
fieldsDecoder =
    field "required" (Decode.list string) |> andThen propertiesDecoder


propertiesDecoder : List String -> Decoder Definition
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
                            [ mapPrimaryKey maybeDesc
                            , mapForeignKey maybeDesc
                            , mapValue PInt int
                            ]

                    Triple "string" _ maybeDesc ->
                        Decode.oneOf
                            [ mapPrimaryKey maybeDesc
                            , mapForeignKey maybeDesc
                            , mapValue PString string
                            ]

                    Triple "boolean" _ _ ->
                        mapValue PBool bool

                    Triple k v _ ->
                        Decode.map BadValue (field "default" Decode.value)
            )


primaryKeyDecoder : Decoder (Maybe PrimaryKey)
primaryKeyDecoder =
    field "default" <| maybe PrimaryKey.decoder


mapPrimaryKey : Maybe String -> Decoder Value
mapPrimaryKey maybeDesc =
    case Maybe.map (Regex.contains primaryKeyRegex) maybeDesc of
        Just True ->
            mapValue PPrimaryKey PrimaryKey.decoder

        _ ->
            Decode.fail ""


mapForeignKey : Maybe String -> Decoder Value
mapForeignKey maybeDesc =
    let
        matchFn =
            List.concatMap .submatches << Regex.find foreignKeyRegex
    in
    case Maybe.map matchFn maybeDesc of
        Just [ Just table, Just col ] ->
            mapValue (PForeignKey ( table, col ) Nothing) PrimaryKey.decoder

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
