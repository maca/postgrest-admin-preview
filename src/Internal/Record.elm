module Internal.Record exposing
    ( Record
    , decoder
    , encode
    , errors
    , fieldToString
    , fromTable
    , getTable
    , hasErrors
    , id
    , label
    , location
    , primaryKey
    , referencedBy
    , setValidation
    , tableName
    , updateWithString
    )

import Dict exposing (Dict)
import Internal.Field as Field exposing (Field)
import Internal.Schema
    exposing
        ( Column
        , Constraint(..)
        , ForeignKeyParams
        , Reference
        , Schema
        , Table
        , valueDecoder
        )
import Internal.Value as Value exposing (Value(..))
import Json.Decode as Decode exposing (Decoder, string)
import Json.Encode as Encode
import Maybe.Extra exposing (isNothing)


type alias Record =
    { table : Table
    , fields : Dict String Field
    , persisted : Bool
    }


fromTable : Table -> Record
fromTable table =
    { table = table
    , fields =
        Dict.map
            (\_ { required, value, constraint } ->
                { constraint = constraint
                , required = required
                , value = value
                , validation = always Nothing
                , changed = False
                , error = Nothing
                }
            )
            table.columns
    , persisted = False
    }


updateWithString : String -> String -> Record -> Record
updateWithString fieldName str record =
    case Dict.get fieldName record.fields of
        Just field ->
            let
                field_ =
                    Field.updateWithString str field
            in
            { record | fields = Dict.insert fieldName field_ record.fields }

        Nothing ->
            record


getTable : Record -> Table
getTable record =
    record.table


id : Record -> Maybe String
id record =
    primaryKey record |> Maybe.andThen (.value >> Value.toString)


location : Record -> Maybe String
location record =
    id record
        |> Maybe.map
            (\i -> "/" ++ tableName record ++ "?id=eq." ++ i)


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


setValidation : (Value -> Maybe String) -> String -> Record -> Record
setValidation validation fieldName record =
    { record
        | fields =
            Dict.map
                (\name field ->
                    if name == fieldName then
                        Field.setValidation validation field

                    else
                        field
                )
                record.fields
    }


hasErrors : Record -> Bool
hasErrors record =
    errors record
        |> Dict.values
        |> List.any (not << isNothing)


errors : Record -> Dict String (Maybe String)
errors record =
    Dict.map
        (\_ field -> .error (Field.validate record.persisted field))
        record.fields


encode : Record -> Encode.Value
encode record =
    record.fields
        |> Dict.filter (\_ { changed } -> changed)
        |> Encode.dict identity (.value >> Value.encode)


decoder : Table -> Decoder Record
decoder table =
    Dict.foldl decoderHelp (Decode.succeed Dict.empty) table.columns
        |> Decode.map
            (\fields ->
                { table = table
                , fields = fields
                , persisted = True
                }
            )


decoderHelp :
    String
    -> Column
    -> Decoder (Dict String Field)
    -> Decoder (Dict String Field)
decoderHelp name column =
    Decode.andThen
        (\dict ->
            Decode.map (Maybe.withDefault dict)
                (Decode.maybe
                    (fieldDecoder name column dict)
                )
        )


fieldDecoder :
    String
    -> Column
    -> Dict String Field
    -> Decoder (Dict String Field)
fieldDecoder name column fields =
    let
        insert constraint value =
            Dict.insert name
                { constraint = constraint
                , required = column.required
                , value = value
                , validation = always Nothing
                , changed = False
                , error = Nothing
                }
                fields

        columnDecoder =
            valueDecoder column.columnType
    in
    case column.constraint of
        ForeignKey params ->
            Decode.map2
                (\referenceLabel ->
                    insert (ForeignKey { params | label = referenceLabel })
                )
                (referenceLabelDecoder params)
                (Decode.field name columnDecoder)

        _ ->
            Decode.map
                (insert column.constraint)
                (Decode.field name columnDecoder)


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
    List.filterMap (labelHelp record) recordIdentifiers |> List.head


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
