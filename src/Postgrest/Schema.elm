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
import Postgrest.Schema.Table exposing (Column, Table)
import Postgrest.Value exposing (Value(..))
import Regex exposing (Regex)
import Task exposing (Task)
import Time.Extra as Time
import Url exposing (Url)
import Url.Builder as Url
import Utils.Task exposing (Error(..), fail, handleJsonResponse, toError)


type alias Schema =
    Dict String Table


type alias ColumnDefinition =
    { type_ : String
    , format : String
    , description : Maybe String
    , enum : List String
    }


getSchema : Url -> Task Error Schema
getSchema url =
    Http.task
        { method = "GET"
        , headers = []
        , url = Url.toString url
        , body = Http.emptyBody
        , resolver = Http.stringResolver <| handleJsonResponse <| decoder
        , timeout = Nothing
        }


decoder : Decoder Schema
decoder =
    columnNamesDecoder
        |> Decode.andThen schemaDecoder


columnNamesDecoder : Decoder (Dict String (List String))
columnNamesDecoder =
    field "definitions"
        (Decode.dict
            (field "properties"
                (Decode.dict Decode.value |> Decode.map Dict.keys)
            )
        )


schemaDecoder : Dict String (List String) -> Decoder Schema
schemaDecoder columnNames =
    field "definitions"
        (Decode.dict
            (field "required" (Decode.list Decode.string)
                |> Decode.andThen
                    (\req ->
                        field "properties" (Decode.keyValuePairs Decode.value)
                            |> Decode.andThen (columnsDecoder columnNames req)
                    )
            )
        )


columnsDecoder :
    Dict String (List String)
    -> List String
    -> List ( String, Decode.Value )
    -> Decoder Table
columnsDecoder columnNames requiredCols definitions =
    let
        results =
            definitions
                |> List.map
                    (\( name, val ) ->
                        let
                            isRequired =
                                List.member name requiredCols
                        in
                        ( name
                        , Decode.decodeValue
                            (columnDecoder columnNames isRequired)
                            val
                        )
                    )
    in
    case List.filterMap (Tuple.second >> toError) results of
        err :: _ ->
            Decode.errorToString err
                |> Decode.fail

        [] ->
            results
                |> List.filterMap
                    (\( n, v ) ->
                        case v of
                            Err _ ->
                                Nothing

                            Ok ok ->
                                Just ( n, ok )
                    )
                |> Dict.fromList
                |> Decode.succeed


columnDecoder : Dict String (List String) -> Bool -> Decoder Column
columnDecoder columnNames isRequired =
    Decode.map4 ColumnDefinition
        (field "type" string)
        (field "format" string)
        (maybe <| field "description" string)
        (Decode.oneOf [ field "enum" (list string), Decode.succeed [] ])
        |> Decode.andThen (columnDecoderHelp columnNames isRequired)


columnDecoderHelp :
    Dict String (List String)
    -> Bool
    -> ColumnDefinition
    -> Decoder Column
columnDecoderHelp columnNames isRequired { type_, format, description, enum } =
    let
        makeColumn val =
            { required = isRequired
            , value = val
            }

        mapValue cons dec =
            Decode.map (cons >> makeColumn)
                (maybe (field "default" dec))

        mapPrimaryKey =
            if isPrimaryKeyDescription description then
                mapValue PPrimaryKey PrimaryKey.decoder

            else
                Decode.fail "Not primary key"

        mapForeignKey =
            let
                matchFn =
                    List.concatMap .submatches << Regex.find foreignKeyRegex
            in
            case Maybe.map matchFn description of
                Just [ Just table, Just primaryKeyName ] ->
                    let
                        colLabel =
                            Dict.get table columnNames
                                |> Maybe.andThen findLabelColum

                        params =
                            { table = table
                            , primaryKeyName = primaryKeyName
                            , labelColumnName = colLabel
                            , label = Nothing
                            }
                    in
                    mapValue (flip PForeignKey params) PrimaryKey.decoder

                _ ->
                    Decode.fail "Not foreign key"
    in
    case type_ of
        "number" ->
            mapValue PFloat Decode.float

        "integer" ->
            Decode.oneOf
                [ mapPrimaryKey, mapForeignKey, mapValue PInt int ]

        "string" ->
            if format == "timestamp without time zone" then
                mapValue PTime Time.decoder

            else if format == "date" then
                mapValue PDate Time.decoder

            else if format == "text" then
                mapValue PText string

            else if not (List.isEmpty enum) then
                mapValue (flip PEnum enum) string

            else
                Decode.oneOf
                    [ mapPrimaryKey, mapForeignKey, mapValue PString string ]

        "boolean" ->
            mapValue PBool bool

        _ ->
            Decode.map (BadValue >> makeColumn) Decode.value


foreignKeyRegex : Regex
foreignKeyRegex =
    Regex.fromString "fk table='(\\w+)' column='(\\w+)'"
        |> Maybe.withDefault Regex.never


isPrimaryKeyDescription : Maybe String -> Bool
isPrimaryKeyDescription mstring =
    Maybe.map (String.contains "Primary Key") mstring |> Maybe.withDefault False



-- To refactor


findLabelColum : List String -> Maybe String
findLabelColum requiredCols =
    List.filter (\n -> List.member n requiredCols) identifiers
        |> List.head


identifiers : List String
identifiers =
    [ "title", "name", "full name", "email", "first name", "last name" ]
