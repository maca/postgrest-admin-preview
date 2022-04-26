module Postgrest.Resource exposing
    ( Resource
    , changed
    , decoder
    , encode
    , errors
    , fieldToString
    , fromTable
    , hasErrors
    , id
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
import Postgrest.PrimaryKey as PrimaryKey exposing (PrimaryKey)
import Postgrest.Schema exposing (Column, Table)
import Postgrest.Value as Value exposing (ForeignKeyParams, Value(..))
import Regex exposing (Regex)


type alias Resource =
    Dict String Field


fromTable : Table -> Resource
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
        table


hasErrors : Resource -> Bool
hasErrors resource =
    errors resource
        |> Dict.values
        |> List.any (not << isNothing)


errors : Resource -> Dict String (Maybe String)
errors resource =
    Dict.map (\_ f -> Field.validate f |> .error) resource


changed : Resource -> Bool
changed resource =
    Dict.values resource |> List.any .changed


encode : Resource -> Encode.Value
encode resource =
    case primaryKeyName resource of
        Just pkName ->
            resource
                |> Dict.remove pkName
                |> Encode.dict identity (.value >> Value.encode)

        Nothing ->
            Encode.null


primaryKeyName : Dict String { a | value : Value } -> Maybe String
primaryKeyName resource =
    Dict.find (\_ v -> Value.isPrimaryKey v.value) resource
        |> Maybe.map Tuple.first


decoder : Table -> Decoder Resource
decoder table =
    Dict.foldl decoderHelp (Decode.succeed Dict.empty) table


decoderHelp : String -> Column -> Decoder Resource -> Decoder Resource
decoderHelp name column result =
    result
        |> Decode.andThen
            (\dict ->
                Decode.oneOf
                    [ fieldDecoder dict name column
                    , Decode.succeed dict
                    ]
            )


fieldDecoder : Resource -> String -> Column -> Decoder Resource
fieldDecoder dict name column =
    let
        insert val =
            dict
                |> Dict.insert name (makeField column val)
                |> Decode.succeed
    in
    case column.value of
        PForeignKey _ params ->
            Decode.map2
                (\label value ->
                    PForeignKey (Just value) { params | label = label }
                )
                (referenceDecoder params)
                (Decode.field name PrimaryKey.decoder)
                |> Decode.andThen insert

        _ ->
            Decode.field name column.decoder
                |> Decode.andThen insert


referenceDecoder : ForeignKeyParams -> Decoder (Maybe String)
referenceDecoder params =
    case params.labelColumnName of
        Just n ->
            Decode.maybe (Decode.at [ params.table, n ] string)

        Nothing ->
            Decode.succeed Nothing


makeField : Column -> Value -> Field
makeField column value =
    { constraint = column.constraint
    , required = column.required
    , error = Nothing
    , changed = False
    , value = value
    }


id : Resource -> Maybe String
id resource =
    primaryKey resource |> Maybe.andThen (.value >> Value.toString)


fieldToString : String -> Resource -> Maybe String
fieldToString key resource =
    Dict.get key resource |> Maybe.andThen (.value >> Value.toString)


primaryKey : Resource -> Maybe Field
primaryKey resource =
    Dict.values resource
        |> List.filter Field.isPrimaryKey
        |> List.head


setError : PG.PostgrestErrorJSON -> Resource -> Resource
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
