module Postgrest.Schema exposing (Schema, decoder)

import Basics.Extra exposing (flip)
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
import Postgrest.PrimaryKey as PrimaryKey exposing (PrimaryKey(..))
import Postgrest.Schema.Definition exposing (Column(..), Definition)
import Postgrest.Value exposing (Value(..))
import Regex exposing (Regex)
import Time.Extra as Time


type alias Schema =
    Dict String Definition


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
        mapColumn ( name, value ) =
            ( name, Column (List.member name required) value )
    in
    Decode.map (Dict.fromList << List.map mapColumn)
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

                    Triple "string" "timestamp without time zone" _ ->
                        mapValue PTime Time.decoder

                    Triple "string" _ maybeDesc ->
                        Decode.oneOf
                            [ mapPrimaryKey maybeDesc
                            , mapForeignKey maybeDesc
                            , mapValue PString string
                            ]

                    Triple "boolean" _ _ ->
                        mapValue PBool bool

                    Triple _ _ _ ->
                        Decode.map BadValue (field "default" Decode.value)
            )


mapValue : (Maybe a -> Value) -> Decoder a -> Decoder Value
mapValue cons dec =
    Decode.map cons (maybe <| field "default" dec)


mapPrimaryKey : Maybe String -> Decoder Value
mapPrimaryKey maybeDesc =
    case Maybe.map (Regex.contains primaryKeyRegex) maybeDesc of
        Just True ->
            mapValue PPrimaryKey PrimaryKey.decoder

        _ ->
            Decode.fail ""


primaryKeyRegex : Regex
primaryKeyRegex =
    Regex.fromString "Primary Key"
        |> Maybe.withDefault Regex.never


mapForeignKey : Maybe String -> Decoder Value
mapForeignKey maybeDesc =
    let
        matchFn =
            List.concatMap .submatches << Regex.find foreignKeyRegex
    in
    case Maybe.map matchFn maybeDesc of
        Just [ Just table, Just primaryKeyName ] ->
            let
                params =
                    { table = table
                    , primaryKeyName = primaryKeyName
                    , labelColumnName = Nothing
                    , label = Nothing
                    }
            in
            mapValue (flip PForeignKey params) PrimaryKey.decoder

        _ ->
            Decode.fail ""


foreignKeyRegex : Regex
foreignKeyRegex =
    Regex.fromString "fk table='(\\w+)' column='(\\w+)'"
        |> Maybe.withDefault Regex.never
