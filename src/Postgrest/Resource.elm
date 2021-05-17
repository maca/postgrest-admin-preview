module Postgrest.Resource exposing
    ( Resource
    , changed
    , decoder
    , encode
    , errors
    , fieldToString
    , hasErrors
    , id
    , primaryKey
    , primaryKeyName
    , setError
    )

import Basics.Extra exposing (flip)
import Dict exposing (Dict)
import Dict.Extra as Dict
import Json.Decode as Decode
    exposing
        ( Decoder
        , bool
        , float
        , int
        , maybe
        , string
        )
import Json.Encode as Encode
import Maybe.Extra as Maybe exposing (isNothing)
import Postgrest.Client as PG
import Postgrest.Field as Field exposing (Field)
import Postgrest.PrimaryKey as PrimaryKey exposing (PrimaryKey)
import Postgrest.Schema.Definition exposing (Column(..), Definition)
import Postgrest.Value as Value exposing (ForeignKeyParams, Value(..))
import Regex exposing (Regex)
import Time.Extra as Time


type alias Resource =
    Dict String Field


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


primaryKeyName : Resource -> Maybe String
primaryKeyName resource =
    Dict.find (\_ v -> Value.isPrimaryKey v.value) resource
        |> Maybe.map Tuple.first


decoder : Definition -> Decoder Resource
decoder definition =
    definition
        |> Dict.foldl (decoderFold definition)
            (Decode.succeed Dict.empty)


id : Resource -> Maybe String
id resource =
    primaryKey resource |> Maybe.map PrimaryKey.toString


fieldToString : String -> Resource -> Maybe String
fieldToString key resource =
    Dict.get key resource |> Maybe.andThen (.value >> Value.toString)


primaryKey : Resource -> Maybe PrimaryKey
primaryKey resource =
    Dict.values resource
        |> List.filterMap (.value >> Value.toPrimaryKey)
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


decoderFold : Definition -> String -> a -> Decoder Resource -> Decoder Resource
decoderFold definition name _ prevDec =
    let
        insert =
            flip (Dict.insert name)

        map makeColumn required dict dec =
            Decode.field name dec
                |> maybe
                |> Decode.map
                    (insert dict << Field Nothing required False << makeColumn)

        foldFun dict =
            case Dict.get name definition of
                Just (Column required (PFloat _)) ->
                    float |> map PFloat required dict

                Just (Column required (PInt _)) ->
                    int |> map PInt required dict

                Just (Column required (PString _)) ->
                    string |> map PString required dict

                Just (Column required (PText _)) ->
                    string |> map PText required dict

                Just (Column required (PEnum _ opts)) ->
                    string |> map (flip PEnum opts) required dict

                Just (Column required (PBool _)) ->
                    bool |> map PBool required dict

                Just (Column required (PTime _)) ->
                    Time.decoder |> map PTime required dict

                Just (Column required (PDate _)) ->
                    Time.decoder |> map PDate required dict

                Just (Column required (PPrimaryKey _)) ->
                    PrimaryKey.decoder |> map PPrimaryKey required dict

                Just (Column required (PForeignKey _ params)) ->
                    let
                        insertFk l pk =
                            insert dict
                                { error = Nothing
                                , required = required
                                , changed = False
                                , value = PForeignKey pk { params | label = l }
                                }
                    in
                    Decode.map2 insertFk
                        (maybe <| referenceDecoder params)
                        (maybe <| Decode.field name PrimaryKey.decoder)

                Just (Column _ (BadValue _)) ->
                    Decode.fail ""

                Nothing ->
                    Decode.fail ""
    in
    Decode.andThen foldFun prevDec


referenceDecoder : ForeignKeyParams -> Decoder String
referenceDecoder params =
    case params.labelColumnName of
        Just n ->
            Decode.at [ params.table, n ] string

        Nothing ->
            Decode.fail ""


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
