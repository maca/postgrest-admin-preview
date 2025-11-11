module Internal.Schema exposing
    ( Schema, Column, ColumnType(..)
    , Constraint(..), ForeignKeyParams, Reference
    , Table, columnNames, label
    , tablePrimaryKey, tablePrimaryKeyName, tablePrimaryKeyValue
    , decoder, valueDecoder
    )

{-|

@docs Schema, Column, ColumnType
@docs Constraint, ForeignKeyParams, Reference
@docs Table, columnNames, label
@docs tablePrimaryKey, tablePrimaryKeyName, tablePrimaryKeyValue
@docs decoder, valueDecoder

-}

import Dict exposing (Dict)
import Internal.Value exposing (Value(..))
import Json.Decode as Decode
import Json.Encode as Encode
import Regex exposing (Regex)
import Set exposing (Set)
import Time.Extra as Time


type ColumnType
    = Integer
    | Float
    | String
    | Text
    | Boolean
    | TimestampWithoutTimezome
    | Timestamp
    | TimeWithoutTimezone
    | Time
    | Date
    | Uuid
    | Json
    | Object
    | Array ColumnType
    | Other String (Maybe String)


type Constraint
    = NoConstraint
    | PrimaryKey
    | ForeignKey ForeignKeyParams


type alias Column =
    { constraint : Constraint
    , required : Bool
    , value : Value
    , columnType : ColumnType
    , options : List Value
    }


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


columnNames : Table -> Set String
columnNames { columns } =
    Set.fromList (Dict.keys columns)


decoder :
    { a | tables : List String, tableAliases : Dict String String }
    -> Decode.Decoder Schema
decoder ({ tableAliases } as params) =
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
                                                    (columnConstraint tableAliases
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
                                    [ Decode.field "default" (valueDecoder type_)
                                    , Decode.succeed (defaultValue type_)
                                    ]
                                )
                                (Decode.oneOf
                                    [ Decode.field "enum" (Decode.list (valueDecoder type_))
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
                                            >> Table tableName
                                            >> Tuple.pair tableName
                                        )
                                )
                        )
                        (Decode.succeed [])
                    |> Decode.map Dict.fromList
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


tablePrimaryKeyValue : Table -> Maybe ( String, Value )
tablePrimaryKeyValue table =
    tablePrimaryKey table |> Maybe.map (Tuple.mapSecond .value)


tablePrimaryKeyName : Table -> Maybe String
tablePrimaryKeyName table =
    tablePrimaryKey table
        |> Maybe.map Tuple.first


labelHelp : Table -> String -> Maybe String
labelHelp table fieldName =
    if Dict.member fieldName table.columns then
        Just fieldName

    else
        Nothing


labelIdentifiers : List String
labelIdentifiers =
    [ "title", "name", "full name", "email", "first name", "last name" ]


columnType : String -> Maybe String -> ColumnType
columnType type_ format =
    case ( type_, format ) of
        ( _, Just "array" ) ->
            Array (columnType type_ Nothing)

        ( "integer", _ ) ->
            Integer

        ( "number", _ ) ->
            Float

        ( "string", Just "timestamp without time zone" ) ->
            TimestampWithoutTimezome

        ( "string", Just "timestamp with time zone" ) ->
            Timestamp

        ( "string", Just "times without time zone" ) ->
            TimeWithoutTimezone

        ( "string", Just "times with time zone" ) ->
            Time

        ( "string", Just "date" ) ->
            Date

        ( "string", Just "uuid" ) ->
            Uuid

        ( "string", Just "json" ) ->
            Json

        ( "string", Just "text" ) ->
            Text

        ( "string", _ ) ->
            String

        ( "boolean", _ ) ->
            Boolean

        ( "object", _ ) ->
            Object

        _ ->
            Other type_ format


valueDecoder : ColumnType -> Decode.Decoder Value
valueDecoder type_ =
    case type_ of
        Integer ->
            Decode.map PInt (Decode.maybe Decode.int)

        Float ->
            Decode.map PFloat (Decode.maybe Decode.float)

        String ->
            Decode.map PString (Decode.maybe Decode.string)

        Text ->
            Decode.map PText (Decode.maybe Decode.string)

        Boolean ->
            Decode.map PBool (Decode.maybe Decode.bool)

        TimestampWithoutTimezome ->
            Decode.map PTime (Decode.maybe Time.decoder)

        Timestamp ->
            Decode.map PTime (Decode.maybe Time.decoder)

        TimeWithoutTimezone ->
            Decode.map PTime (Decode.maybe Time.decoder)

        Time ->
            Decode.map PTime (Decode.maybe Time.decoder)

        Date ->
            Decode.map PDate (Decode.maybe Time.decoder)

        Uuid ->
            Decode.map PString (Decode.maybe Decode.string)

        Json ->
            Decode.map (PJson << Maybe.map (Encode.encode 4)) (Decode.maybe Decode.value)

        Object ->
            Decode.map Unknown Decode.value

        Array _ ->
            Decode.map Unknown Decode.value

        Other _ _ ->
            Decode.map Unknown Decode.value


defaultValue : ColumnType -> Value
defaultValue type_ =
    case type_ of
        Integer ->
            PInt Nothing

        Float ->
            PFloat Nothing

        String ->
            PString Nothing

        Text ->
            PText Nothing

        Boolean ->
            PBool Nothing

        TimestampWithoutTimezome ->
            PTime Nothing

        Timestamp ->
            PTime Nothing

        TimeWithoutTimezone ->
            PTime Nothing

        Time ->
            PTime Nothing

        Date ->
            PDate Nothing

        Uuid ->
            PString Nothing

        Json ->
            PJson Nothing

        Object ->
            Unknown Encode.null

        Array _ ->
            Unknown Encode.null

        Other _ _ ->
            Unknown Encode.null


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
                , primaryKeyName = primaryKeyName
                , labelColumnName =
                    Dict.get table colNames
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


label : Table -> Maybe String
label table =
    List.filterMap (labelHelp table) labelIdentifiers
        |> List.head



-- To refactor


findLabelColum : List String -> Maybe String
findLabelColum requiredCols =
    List.filter (\n -> List.member n requiredCols) identifiers
        |> List.head


identifiers : List String
identifiers =
    [ "title", "name", "full name", "email", "first name", "last name" ]
