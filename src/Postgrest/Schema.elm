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
    let
        makeDefinition partials _ ( requiredCols, values ) =
            let
                makeColumn name value =
                    let
                        isRequired =
                            List.member name requiredCols
                    in
                    case value of
                        PForeignKey fk params ->
                            let
                                col =
                                    Dict.get params.table partials
                                        |> Maybe.andThen
                                            (Tuple.first >> findLabelColum)
                            in
                            PForeignKey fk { params | labelColumnName = col }
                                |> Column isRequired

                        _ ->
                            Column isRequired value
            in
            Dict.map makeColumn values

        fieldsDecoder =
            Decode.map2 Tuple.pair
                (field "required" <| Decode.list string)
                (field "properties" <| Decode.dict valueDecoder)
    in
    Decode.map (\partials -> Dict.map (makeDefinition partials) partials)
        (field "definitions" <| Decode.dict fieldsDecoder)


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

                    Triple "string" "date" _ ->
                        mapValue PDate Time.decoder

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



-- To refactor


findLabelColum : List String -> Maybe String
findLabelColum requiredCols =
    List.filter (\n -> List.member n requiredCols) identifiers
        |> List.head


identifiers : List String
identifiers =
    [ "title", "name", "full name", "email", "first name", "last name" ]
