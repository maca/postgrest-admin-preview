module Record exposing (Record, decoder, encode, id, primaryKey, primaryKeyName)

import Basics.Extra exposing (curry, flip)
import Dict exposing (Dict)
import Dict.Extra as Dict
import Field exposing (Field)
import Iso8601
import Json.Decode as Decode
    exposing
        ( Decoder
        , bool
        , decodeValue
        , float
        , int
        , maybe
        , nullable
        , string
        )
import Json.Encode as Encode
import Postgrest.Client as PG
import PrimaryKey exposing (PrimaryKey)
import Schema.Definition exposing (Column(..), Definition)
import Time.Extra as Time
import Value exposing (Value(..))


type alias Record =
    Dict String Field


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

        map cons dict dec =
            Decode.field name dec |> Decode.map (insert dict << cons)

        foldFun dict =
            case Dict.get name definition of
                Just (Column required (PFloat _)) ->
                    maybe float |> map (Field Nothing required << PFloat) dict

                Just (Column required (PInt _)) ->
                    Debug.todo "crash"

                -- maybe int |> map PInt dict
                Just (Column required (PString _)) ->
                    Debug.todo "crash"

                -- maybe string |> map PString dict
                Just (Column required (PBool _)) ->
                    Debug.todo "crash"

                -- maybe bool |> map PBool dict
                Just (Column required (PTime _)) ->
                    Debug.todo "crash"

                -- maybe Time.decoder |> map PTime dict
                Just (Column required (PPrimaryKey _)) ->
                    Debug.todo "crash"

                -- maybe PrimaryKey.decoder
                --     |> map PPrimaryKey dict
                Just (Column required (PForeignKey ( table, col ) _ _)) ->
                    Debug.todo "crash"

                -- let
                --     mapFun d pk =
                --         insert dict <| PForeignKey ( table, col ) d pk
                --     refDec i =
                --         Decode.at [ table, i ] (nullable string)
                -- in
                -- Decode.map2 mapFun
                --     (Decode.oneOf <| List.map refDec identifiers)
                --     (maybe <| Decode.field name PrimaryKey.decoder)
                Just (Column required (BadValue _)) ->
                    Debug.todo "crash"

                -- map BadValue dict Decode.value
                Nothing ->
                    Debug.todo "crash"

        -- map BadValue dict Decode.value
    in
    Decode.andThen foldFun prevDec
