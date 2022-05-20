module Postgrest.Record exposing
    ( Record
    , decoder
    , encode
    , errors
    , fieldToString
    , fromTable
    , hasErrors
    , id
    , label
    , primaryKey
    , primaryKeyName
    , referencedBy
    , tableName
    , toTable
    )

import Dict exposing (Dict)
import Dict.Extra as Dict
import Json.Decode as Decode exposing (Decoder, maybe, string)
import Json.Encode as Encode
import Maybe.Extra as Maybe exposing (isNothing)
import Postgrest.Field as Field exposing (Field)
import Postgrest.Schema
    exposing
        ( Column
        , Constraint(..)
        , ForeignKeyParams
        , Reference
        , Schema
        , Table
        )
import Postgrest.Value as Value exposing (Value(..))


type alias Record =
    { table : Table
    , fields : Dict String Field
    }


fromTable : Table -> Record
fromTable table =
    { table = table
    , fields =
        Dict.map
            (\_ { required, value, constraint } ->
                { required = required
                , constraint = constraint
                , error = Nothing
                , changed = False
                , value = value
                }
            )
            table.columns
    }


toTable : Record -> Table
toTable record =
    record.table


id : Record -> Maybe String
id record =
    primaryKey record |> Maybe.andThen (.value >> Value.toString)


tableName : Record -> String
tableName record =
    record.table.name


fieldToString : String -> Record -> Maybe String
fieldToString key record =
    Dict.get key record.fields |> Maybe.andThen (.value >> Value.toString)


primaryKey : Record -> Maybe Field
primaryKey record =
    Dict.values record.fields
        |> List.filter Field.isPrimaryKey
        |> List.head


hasErrors : Record -> Bool
hasErrors record =
    errors record
        |> Dict.values
        |> List.any (not << isNothing)


errors : Record -> Dict String (Maybe String)
errors record =
    Dict.map (\_ f -> Field.validate f |> .error) record.fields


encode : Record -> Encode.Value
encode record =
    Encode.dict identity (.value >> Value.encode) record.fields


primaryKeyName : Record -> Maybe String
primaryKeyName record =
    Dict.find (\_ column -> Field.isPrimaryKey column) record.fields
        |> Maybe.map Tuple.first


decoder : Table -> Decoder Record
decoder table =
    Dict.foldl decoderHelp (Decode.succeed Dict.empty) table.columns
        |> Decode.map (\fields -> { table = table, fields = fields })


decoderHelp :
    String
    -> Column
    -> Decoder (Dict String Field)
    -> Decoder (Dict String Field)
decoderHelp name column =
    Decode.andThen
        (\dict ->
            Decode.oneOf
                [ fieldDecoder dict name column
                , Decode.succeed dict
                ]
        )


fieldDecoder :
    Dict String Field
    -> String
    -> Column
    -> Decoder (Dict String Field)
fieldDecoder fields name column =
    let
        insert constraint value =
            Dict.insert name
                { constraint = constraint
                , required = column.required
                , error = Nothing
                , changed = False
                , value = value
                }
                fields
    in
    case column.constraint of
        ForeignKey params ->
            Decode.map2
                (\referenceLabel ->
                    insert (ForeignKey { params | label = referenceLabel })
                )
                (referenceLabelDecoder params)
                (Decode.field name column.decoder)

        _ ->
            Decode.map
                (insert column.constraint)
                (Decode.field name column.decoder)


referenceLabelDecoder : ForeignKeyParams -> Decoder (Maybe String)
referenceLabelDecoder params =
    case params.labelColumnName of
        Just columnName ->
            Decode.maybe (Decode.at [ params.tableName, columnName ] string)

        Nothing ->
            Decode.succeed Nothing


referencedBy : Schema -> Record -> List Reference
referencedBy schema record =
    Dict.foldl
        (\_ table acc ->
            Dict.foldl
                (\columnName column columns ->
                    case column.constraint of
                        ForeignKey foreignKey ->
                            if foreignKey.tableName == tableName record then
                                { foreignKeyName = columnName
                                , foreignKeyValue =
                                    record.fields
                                        |> Dict.get foreignKey.primaryKeyName
                                        |> Maybe.andThen
                                            (.value >> Value.toString)
                                        |> Maybe.withDefault ""
                                , table = table
                                }
                                    :: columns

                            else
                                columns

                        _ ->
                            columns
                )
                acc
                table.columns
        )
        []
        schema


label : Record -> Maybe String
label record =
    case
        List.filterMap (labelHelp record) recordIdentifiers |> List.head
    of
        Just recordLabel ->
            Just recordLabel

        Nothing ->
            id record


labelHelp : Record -> String -> Maybe String
labelHelp record fieldName =
    case Dict.get fieldName record.fields |> Maybe.map .value of
        Just (PString recordLabel) ->
            recordLabel

        _ ->
            Nothing



-- To refactor


recordIdentifiers : List String
recordIdentifiers =
    [ "title", "name", "full name", "email", "first name", "last name" ]
