module Postgrest.Schema exposing
    ( Column
    , Constraint(..)
    , ForeignKeyParams
    , Reference
    , Schema
    , Table
    , decoder
    , fetchSchema
    )

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
import Postgrest.Value exposing (Value(..))
import Regex exposing (Regex)
import Task exposing (Task)
import Time.Extra as Time
import Url exposing (Url)
import Url.Builder as Url
import Utils.Task exposing (Error(..), fail, handleJsonResponse, toError)


type alias ColumnNames =
    Dict String (List String)


type alias Column =
    { constraint : Constraint
    , required : Bool
    , decoder : Decoder Value
    , value : Value
    }


type Constraint
    = NoConstraint
    | PrimaryKey
    | ForeignKey ForeignKeyParams


type alias ForeignKeyParams =
    { tableName : String
    , primaryKeyName : String
    , labelColumnName : Maybe String
    , label : Maybe String
    }


type alias Table =
    { name : String
    , columns : Dict String Column
    }


type alias Reference =
    { foreignKeyName : String
    , foreignKeyValue : String
    , table : Table
    }


type alias Schema =
    Dict String Table


type alias ColumnDefinition =
    { type_ : String
    , format : String
    , description : Maybe String
    , enum : List String
    }


fetchSchema : Url -> Task Error Schema
fetchSchema url =
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


columnNamesDecoder : Decoder ColumnNames
columnNamesDecoder =
    field "definitions"
        (Decode.dict
            (field "properties"
                (Decode.dict (Decode.succeed ()) |> Decode.map Dict.keys)
            )
        )


schemaDecoder : ColumnNames -> Decoder Schema
schemaDecoder columnNames =
    field "definitions" (Decode.keyValuePairs Decode.value)
        |> Decode.andThen (decodeResults (tableDecoder columnNames))


tableDecoder : ColumnNames -> String -> Decoder Table
tableDecoder columnNames tableName =
    field "required" (Decode.list Decode.string)
        |> Decode.andThen
            (\requiredColumns ->
                field "properties" (Decode.keyValuePairs Decode.value)
                    |> Decode.andThen
                        (decodeResults
                            (columnDecoder columnNames requiredColumns)
                        )
            )
        |> Decode.map (\columns -> { columns = columns, name = tableName })


columnDecoder : ColumnNames -> List String -> String -> Decoder Column
columnDecoder columnNames requiredColumns columnName =
    Decode.map4 ColumnDefinition
        (field "type" string)
        (field "format" string)
        (maybe <| field "description" string)
        (Decode.oneOf [ field "enum" (list string), Decode.succeed [] ])
        |> Decode.andThen
            (\{ type_, format, description, enum } ->
                let
                    makeColumn valueDecoder val =
                        { constraint =
                            Maybe.map (columnConstraint columnNames) description
                                |> Maybe.withDefault NoConstraint
                        , required = List.member columnName requiredColumns
                        , decoder = valueDecoder
                        , value = val
                        }

                    mapValue cons dec =
                        let
                            valueDecoder =
                                Decode.map cons (Decode.maybe dec)
                        in
                        Decode.map (makeColumn valueDecoder)
                            (Decode.oneOf
                                [ field "default" valueDecoder
                                , Decode.succeed (cons Nothing)
                                ]
                            )
                in
                case type_ of
                    "number" ->
                        mapValue PFloat Decode.float

                    "integer" ->
                        mapValue PInt int

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
                            mapValue PString string

                    "boolean" ->
                        mapValue PBool bool

                    _ ->
                        let
                            valueDecoder =
                                Decode.map Unknown Decode.value
                        in
                        Decode.map (makeColumn valueDecoder) valueDecoder
            )


decodeResults :
    (String -> Decoder value)
    -> List ( String, Decode.Value )
    -> Decoder (Dict String value)
decodeResults valueDecoder values =
    let
        results =
            List.map
                (\( name, value ) ->
                    Decode.decodeValue (valueDecoder name) value
                        |> Result.map (Tuple.pair name)
                )
                values
    in
    case List.filterMap toError results of
        err :: _ ->
            Decode.fail (Decode.errorToString err)

        [] ->
            results
                |> List.filterMap (Result.map Just >> Result.withDefault Nothing)
                |> Dict.fromList
                |> Decode.succeed


columnConstraint : ColumnNames -> String -> Constraint
columnConstraint columnNames description =
    case extractForeignKey description of
        [ tableName, primaryKeyName ] ->
            ForeignKey
                { tableName = tableName
                , primaryKeyName = primaryKeyName
                , labelColumnName =
                    Dict.get tableName columnNames
                        |> Maybe.andThen findLabelColum
                , label = Nothing
                }

        _ ->
            if isPrimaryKeyDescription description then
                PrimaryKey

            else
                NoConstraint


foreignKeyRegex : Regex
foreignKeyRegex =
    Regex.fromString "fk table='(\\w+)' column='(\\w+)'"
        |> Maybe.withDefault Regex.never


isPrimaryKeyDescription : String -> Bool
isPrimaryKeyDescription description =
    String.contains "Primary Key" description


extractForeignKey : String -> List String
extractForeignKey description =
    Regex.find foreignKeyRegex description
        |> List.concatMap .submatches
        |> List.filterMap identity



-- To refactor


findLabelColum : List String -> Maybe String
findLabelColum requiredCols =
    List.filter (\n -> List.member n requiredCols) identifiers
        |> List.head


identifiers : List String
identifiers =
    [ "title", "name", "full name", "email", "first name", "last name" ]
