module Record exposing
    ( Record
    , changed
    , decoder
    , encode
    , errors
    , hasErrors
    , id
    , primaryKey
    , primaryKeyName
    , setError
    )

import Basics.Extra exposing (flip)
import Dict exposing (Dict)
import Dict.Extra as Dict
import Field exposing (Field)
import Json.Decode as Decode
    exposing
        ( Decoder
        , bool
        , float
        , int
        , maybe
        , nullable
        , string
        )
import Json.Encode as Encode
import Maybe.Extra exposing (isNothing)
import Postgrest.Client as PG
import PrimaryKey exposing (PrimaryKey)
import Regex exposing (Regex)
import Schema.Definition exposing (Column(..), Definition)
import Time.Extra as Time
import Value exposing (Value(..))


type alias Record =
    Dict String Field


hasErrors : Record -> Bool
hasErrors record =
    errors record
        |> Dict.values
        |> List.any (not << isNothing)


errors : Record -> Dict String (Maybe String)
errors record =
    Dict.map (\_ f -> Field.validate f |> .error) record


changed : Record -> Bool
changed record =
    Dict.values record |> List.any .changed


encode : Record -> Encode.Value
encode record =
    case primaryKeyName record of
        Just pkName ->
            record
                |> Dict.remove pkName
                |> Encode.dict identity (.value >> Value.encode)

        Nothing ->
            Encode.null


primaryKeyName : Record -> Maybe String
primaryKeyName record =
    Dict.find (\_ v -> Value.isPrimaryKey v.value) record
        |> Maybe.map Tuple.first


decoder : List String -> Definition -> Decoder Record
decoder identifiers definition =
    definition
        |> Dict.foldl (decoderFold identifiers definition)
            (Decode.succeed Dict.empty)


id : Record -> Maybe String
id record =
    primaryKey record |> Maybe.map PrimaryKey.toString


primaryKey : Record -> Maybe PrimaryKey
primaryKey record =
    Dict.values record
        |> List.filterMap (.value >> Value.toPrimaryKey)
        |> List.head


setError : PG.PostgrestErrorJSON -> Record -> Record
setError { code, message } record =
    case code of
        Just "23502" ->
            let
                mapFun columnName key field =
                    if key == columnName then
                        { field | error = Just "This field is required" }

                    else
                        field
            in
            message
                |> Maybe.andThen extractColumnName
                |> Maybe.map (mapFun >> flip Dict.map record)
                |> Maybe.withDefault record

        _ ->
            record


decoderFold :
    List String
    -> Definition
    -> String
    -> a
    -> Decoder Record
    -> Decoder Record
decoderFold identifiers definition name _ prevDec =
    let
        insert =
            flip (Dict.insert name)

        map cons required dict dec =
            Decode.field name dec
                |> Decode.map
                    (insert dict
                        << Field Nothing required False
                        << cons
                    )

        foldFun dict =
            case Dict.get name definition of
                Just (Column required (PFloat _)) ->
                    maybe float |> map PFloat required dict

                Just (Column required (PInt _)) ->
                    maybe int |> map PInt required dict

                Just (Column required (PString _)) ->
                    maybe string |> map PString required dict

                Just (Column required (PBool _)) ->
                    maybe bool |> map PBool required dict

                Just (Column required (PTime _)) ->
                    maybe Time.decoder |> map PTime required dict

                Just (Column required (PPrimaryKey _)) ->
                    maybe PrimaryKey.decoder |> map PPrimaryKey required dict

                Just (Column required (PForeignKey ( table, col ) _ _)) ->
                    let
                        mapFun d pk =
                            insert dict <|
                                Field Nothing required False <|
                                    PForeignKey ( table, col ) d pk

                        refDec i =
                            Decode.at [ table, i ] (nullable string)
                    in
                    Decode.map2 mapFun
                        (Decode.oneOf <| List.map refDec identifiers)
                        (maybe <| Decode.field name PrimaryKey.decoder)

                Just (Column required (BadValue _)) ->
                    map BadValue required dict Decode.value

                Nothing ->
                    map BadValue False dict Decode.value
    in
    Decode.andThen foldFun prevDec


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
