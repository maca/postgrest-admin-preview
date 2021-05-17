module Postgrest.Schema exposing (Schema, decoder, getSchema)

import Basics.Extra exposing (flip)
import Dict exposing (Dict)
import Http
import Json.Decode as Decode
    exposing
        ( Decoder
        , andThen
        , bool
        , field
        , float
        , int
        , list
        , maybe
        , string
        )
import Postgrest.PrimaryKey as PrimaryKey exposing (PrimaryKey(..))
import Postgrest.Schema.Definition exposing (Column(..), Definition)
import Postgrest.Value exposing (Value(..))
import Regex exposing (Regex)
import Task exposing (Task)
import Time.Extra as Time
import Utils.Task exposing (Error(..), fail, handleJsonResponse)


type alias Schema =
    Dict String Definition


type Cuadruple a b c d
    = Cuadruple a b c d


getSchema : String -> Task Error Schema
getSchema host =
    Http.task
        { method = "GET"
        , headers = []
        , url = host
        , body = Http.emptyBody
        , resolver = Http.stringResolver <| handleJsonResponse <| decoder
        , timeout = Nothing
        }


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
    Decode.map4 Cuadruple
        (field "type" string)
        (field "format" string)
        (maybe <| field "description" string)
        (maybe <| field "enum" (list string))
        |> andThen
            (\data ->
                case data of
                    Cuadruple "number" _ _ _ ->
                        mapValue PFloat float

                    Cuadruple "integer" _ maybeDesc _ ->
                        Decode.oneOf
                            [ mapPrimaryKey maybeDesc
                            , mapForeignKey maybeDesc
                            , mapValue PInt int
                            ]

                    Cuadruple "string" "timestamp without time zone" _ _ ->
                        mapValue PTime Time.decoder

                    Cuadruple "string" "date" _ _ ->
                        mapValue PDate Time.decoder

                    Cuadruple "string" "text" _ _ ->
                        mapValue PText string

                    Cuadruple "string" _ _ (Just enum) ->
                        mapValue (flip PEnum enum) string

                    Cuadruple "string" _ maybeDesc _ ->
                        Decode.oneOf
                            [ mapPrimaryKey maybeDesc
                            , mapForeignKey maybeDesc
                            , mapValue PString string
                            ]

                    Cuadruple "boolean" _ _ _ ->
                        mapValue PBool bool

                    Cuadruple _ _ _ _ ->
                        Decode.map BadValue Decode.value
            )


mapValue : (Maybe a -> Value) -> Decoder a -> Decoder Value
mapValue makeValue dec =
    Decode.map makeValue (maybe <| field "default" dec)


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
