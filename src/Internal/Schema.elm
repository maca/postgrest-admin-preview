module Internal.Schema exposing
    ( Schema, Column, ColumnType(..)
    , Constraint(..), Reference
    , Table, label
    , tablePrimaryKey, tablePrimaryKeyName
    , tableToSortedColumnList
    , decoder, valueDecoder
    , Record, Value(..)
    , buildParentReference, buildReferences
    , valueToString
    )

{-|

@docs Schema, Column, ColumnType
@docs Constraint, ForeignKeyParams, Reference
@docs Table, columnNames, label
@docs tablePrimaryKey, tablePrimaryKeyName
@docs tableToSortedColumnList
@docs decoder, valueDecoder
@docs Record, Value
@docs buildParentReference, buildReferences

-}

import Dict exposing (Dict)
import Json.Decode as Decode
import Regex exposing (Regex)


type ColumnType
    = IntegerCol
    | FloatCol
    | StringCol
    | TextCol
    | BoolCol
    | TimestampWithoutTimezomeCol
    | TimestampCol
    | TimeWithoutTimezoneCol
    | TimeCol
    | DateCol
    | UuidCol
    | JsonCol
    | ObjectCol
    | ArrayCol ColumnType
    | OtherCol String (Maybe String)


type Constraint
    = NoConstraint
    | PrimaryKey
    | ForeignKey Reference


type alias Column =
    { constraint : Constraint
    , required : Bool
    , value : Value
    , columnType : ColumnType
    , options : List Value
    }


type alias Reference =
    { tableName : String
    , foreignKey : String
    , labelColumn : Maybe String
    }


type alias Table =
    { referencedBy : List Reference
    , name : String
    , columns : Dict String Column
    }


type Value
    = String String
    | Bool Bool
    | Int Int
    | Float Float
    | Ref { tableName : String, primaryKey : String, label : Maybe String }
    | Blank


valueToString : Value -> Maybe String
valueToString value =
    case value of
        String str ->
            Just str

        Bool True ->
            Just "true"

        Bool False ->
            Just "false"

        Int int ->
            Just (String.fromInt int)

        Float float ->
            Just (String.fromFloat float)

        Ref ref ->
            List.filterMap identity [ ref.label, Just ref.primaryKey ]
                |> String.join " - "
                |> Just

        Blank ->
            Nothing


type alias Record =
    Dict String Value


type alias Schema =
    Dict String Table


tableToSortedColumnList : Table -> List ( String, Column )
tableToSortedColumnList table =
    table.columns
        |> Dict.toList
        |> List.sortBy
            (\( _, column ) ->
                case column.constraint of
                    PrimaryKey ->
                        0

                    ForeignKey _ ->
                        1

                    NoConstraint ->
                        2
            )


decoder :
    { a | tables : List String, tableAliases : Dict String String }
    -> Decode.Decoder Schema
decoder params =
    let
        columnDecoder definitions requiredCols tableName columnName =
            Decode.at [ "definitions", tableName, "properties", columnName ]
                (Decode.field "type" Decode.string
                    |> Decode.andThen
                        (\type_ ->
                            Decode.maybe (Decode.field "format" Decode.string)
                                |> Decode.map (columnType type_)
                        )
                    |> Decode.andThen
                        (\type_ ->
                            Decode.map3
                                (\value options description ->
                                    ( columnName
                                    , { constraint =
                                            description
                                                |> Maybe.map
                                                    (columnConstraint params.tableAliases
                                                        (definitions
                                                            |> Dict.map (always .properties)
                                                        )
                                                    )
                                                |> Maybe.withDefault NoConstraint
                                      , required = List.member columnName requiredCols
                                      , value = value
                                      , options = options
                                      , columnType = type_
                                      }
                                    )
                                )
                                (Decode.oneOf
                                    [ Decode.field "default" valueDecoder
                                    , Decode.succeed Blank
                                    ]
                                )
                                (Decode.oneOf
                                    [ Decode.field "enum" (Decode.list valueDecoder)
                                    , Decode.succeed []
                                    ]
                                )
                                (Decode.maybe (Decode.field "description" Decode.string))
                        )
                )
    in
    Decode.field "definitions"
        (Decode.dict
            (Decode.map2 (\p r -> { properties = p, requiredCols = r })
                (Decode.field "properties"
                    (Decode.dict (Decode.succeed ()) |> Decode.map Dict.keys)
                )
                (Decode.oneOf
                    [ Decode.field "required" (Decode.list Decode.string)
                    , Decode.succeed []
                    ]
                )
            )
            |> Decode.map
                (Dict.filter
                    (\k _ -> List.isEmpty params.tables || List.member k params.tables)
                )
        )
        |> Decode.andThen
            (\definitions ->
                Dict.toList definitions
                    |> List.foldl
                        (\( tableName, { properties, requiredCols } ) ->
                            Decode.map2 (::)
                                ((properties
                                    |> List.foldl
                                        (\columnName ->
                                            Decode.map2 (::)
                                                (columnDecoder definitions requiredCols tableName columnName)
                                        )
                                        (Decode.succeed [])
                                 )
                                    |> Decode.map
                                        (Dict.fromList
                                            >> Table [] tableName
                                            >> Tuple.pair tableName
                                        )
                                )
                        )
                        (Decode.succeed [])
                    |> Decode.map Dict.fromList
            )
        |> Decode.map
            (\schema ->
                Dict.map
                    (\_ table ->
                        { table | referencedBy = buildReferencedBy schema table }
                    )
                    schema
            )


tablePrimaryKey : Table -> Maybe ( String, Column )
tablePrimaryKey { columns } =
    columns
        |> Dict.filter
            (\_ { constraint } ->
                case constraint of
                    PrimaryKey ->
                        True

                    _ ->
                        False
            )
        |> Dict.toList
        |> List.head


tablePrimaryKeyName : Table -> Maybe String
tablePrimaryKeyName table =
    tablePrimaryKey table
        |> Maybe.map Tuple.first


columnType : String -> Maybe String -> ColumnType
columnType type_ format =
    case ( type_, format ) of
        ( _, Just "array" ) ->
            ArrayCol (columnType type_ Nothing)

        ( "integer", _ ) ->
            IntegerCol

        ( "number", _ ) ->
            FloatCol

        ( "string", Just "timestamp without time zone" ) ->
            TimestampWithoutTimezomeCol

        ( "string", Just "timestamp with time zone" ) ->
            TimestampCol

        ( "string", Just "times without time zone" ) ->
            TimeWithoutTimezoneCol

        ( "string", Just "times with time zone" ) ->
            TimeCol

        ( "string", Just "date" ) ->
            DateCol

        ( "string", Just "uuid" ) ->
            UuidCol

        ( "string", Just "json" ) ->
            JsonCol

        ( "string", Just "text" ) ->
            TextCol

        ( "string", Just "bytea" ) ->
            OtherCol type_ format

        ( "string", Just "tsvector" ) ->
            OtherCol type_ format

        ( "string", _ ) ->
            StringCol

        ( "boolean", _ ) ->
            BoolCol

        ( "object", _ ) ->
            ObjectCol

        _ ->
            OtherCol type_ format


valueDecoder : Decode.Decoder Value
valueDecoder =
    Decode.oneOf
        [ Decode.string |> Decode.map String
        , Decode.bool |> Decode.map Bool
        , Decode.int |> Decode.map Int
        , Decode.float |> Decode.map Float
        , Decode.null Blank
        ]


columnConstraint : Dict String String -> Dict String (List String) -> String -> Constraint
columnConstraint tableAliases colNames description =
    case extractForeignKey description of
        [ tableName, primaryKeyName ] ->
            let
                table =
                    Dict.get tableName tableAliases
                        |> Maybe.withDefault tableName
            in
            ForeignKey
                { tableName = table
                , foreignKey = primaryKeyName
                , labelColumn =
                    Dict.get table colNames
                        |> Maybe.andThen
                            (\requiredCols ->
                                List.filter
                                    (\n -> List.member n requiredCols)
                                    identifiers
                                    |> List.head
                            )
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


label : Table -> Maybe String
label table =
    List.filterMap
        (\fieldName ->
            if Dict.member fieldName table.columns then
                Just fieldName

            else
                Nothing
        )
        identifiers
        |> List.head



-- REFERENCES


buildReferencedBy : Schema -> Table -> List Reference
buildReferencedBy schema table =
    Dict.foldl
        (\_ otherTable acc ->
            Dict.foldl
                (\columnName column columnsAcc ->
                    case column.constraint of
                        ForeignKey foreignKey ->
                            if foreignKey.tableName == table.name then
                                { foreignKey = columnName
                                , tableName = otherTable.name
                                , labelColumn = foreignKey.labelColumn
                                }
                                    :: columnsAcc

                            else
                                columnsAcc

                        _ ->
                            columnsAcc
                )
                acc
                otherTable.columns
        )
        []
        schema


buildReferences : Table -> Dict String Reference
buildReferences table =
    Dict.foldl
        (\colName column acc ->
            case column.constraint of
                ForeignKey foreignKey ->
                    Dict.insert colName foreignKey acc

                _ ->
                    acc
        )
        Dict.empty
        table.columns


buildParentReference :
    Schema
    -> Table
    ->
        ({ id : String, tableName : String }
         ->
            Maybe
                { parentTable : Table
                , parentPrimaryKey : String
                , parentId : String
                , parentLabelColumn : String
                }
        )
buildParentReference schema table parent =
    Maybe.map2
        (\parentTable ref ->
            Maybe.map
                (\labelColumn ->
                    { parentTable = parentTable
                    , parentPrimaryKey = ref.foreignKey
                    , parentId = parent.id
                    , parentLabelColumn = labelColumn
                    }
                )
                ref.labelColumn
        )
        (Dict.get parent.tableName schema)
        (buildReferences table
            |> Dict.values
            |> List.filter (\f -> parent.tableName == f.tableName)
            |> List.head
        )
        |> Maybe.andThen identity



-- To refactor


identifiers : List String
identifiers =
    [ "title"
    , "name"
    , "full name"
    , "email"
    , "first name"
    , "last name"
    , "city"
    , "country"
    ]
