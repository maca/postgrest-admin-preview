module Postgrest.Record exposing
    ( Record
    , changed
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
    , setError
    )

import Basics.Extra exposing (flip)
import Dict exposing (Dict)
import Dict.Extra as Dict
import Json.Decode as Decode exposing (Decoder, maybe, string)
import Json.Encode as Encode
import Maybe.Extra as Maybe exposing (isNothing)
import Postgrest.Client as PG
import Postgrest.Field as Field exposing (Field)
import Postgrest.Schema
    exposing
        ( Column
        , Constraint(..)
        , ForeignKeyParams
        , Table
        )
import Postgrest.Value as Value exposing (Value(..))
import Regex exposing (Regex)


type alias Record =
    Dict String Field


fromTable : Table -> Record
fromTable table =
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


hasErrors : Record -> Bool
hasErrors resource =
    errors resource
        |> Dict.values
        |> List.any (not << isNothing)


errors : Record -> Dict String (Maybe String)
errors resource =
    Dict.map (\_ f -> Field.validate f |> .error) resource


changed : Record -> Bool
changed resource =
    Dict.values resource |> List.any .changed


encode : Record -> Encode.Value
encode resource =
    Encode.dict identity (.value >> Value.encode) resource


primaryKeyName : Dict String { a | constraint : Constraint } -> Maybe String
primaryKeyName resource =
    Dict.find (\_ column -> Field.isPrimaryKey column) resource
        |> Maybe.map Tuple.first


decoder : Table -> Decoder Record
decoder table =
    Dict.foldl decoderHelp (Decode.succeed Dict.empty) table.columns


decoderHelp : String -> Column -> Decoder Record -> Decoder Record
decoderHelp name column result =
    result
        |> Decode.andThen
            (\dict ->
                Decode.oneOf
                    [ fieldDecoder dict name column
                    , Decode.succeed dict
                    ]
            )


fieldDecoder : Record -> String -> Column -> Decoder Record
fieldDecoder resource name column =
    let
        insert constraint value =
            Dict.insert name
                { constraint = constraint
                , required = column.required
                , error = Nothing
                , changed = False
                , value = value
                }
                resource
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


id : Record -> Maybe String
id resource =
    primaryKey resource |> Maybe.andThen (.value >> Value.toString)


fieldToString : String -> Record -> Maybe String
fieldToString key resource =
    Dict.get key resource |> Maybe.andThen (.value >> Value.toString)


primaryKey : Record -> Maybe Field
primaryKey resource =
    Dict.values resource
        |> List.filter Field.isPrimaryKey
        |> List.head


setError : PG.PostgrestErrorJSON -> Record -> Record
setError error resource =
    let
        mapFun columnName key field =
            if key == columnName then
                Field.setError error field

            else
                field
    in
    error.message
        |> Maybe.andThen extractColumnName
        |> Maybe.map (mapFun >> flip Dict.map resource)
        |> Maybe.withDefault resource


extractColumnName : String -> Maybe String
extractColumnName string =
    Regex.find columnRegex string
        |> List.head
        |> Maybe.andThen (.submatches >> List.head)
        |> Maybe.withDefault Nothing


columnRegex : Regex
columnRegex =
    Regex.fromString "column \"(\\w+)\""
        |> Maybe.withDefault Regex.never


label : Record -> Maybe String
label resource =
    case
        List.filterMap (labelHelp resource) resourceIdentifiers |> List.head
    of
        Just resourceLabel ->
            Just resourceLabel

        Nothing ->
            id resource


labelHelp : Record -> String -> Maybe String
labelHelp resource fieldName =
    case Dict.get fieldName resource |> Maybe.map .value of
        Just (PString resourceLabel) ->
            resourceLabel

        _ ->
            Nothing



-- To refactor


resourceIdentifiers : List String
resourceIdentifiers =
    [ "title", "name", "full name", "email", "first name", "last name" ]
