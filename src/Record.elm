module Record exposing (Record, decoder, encode, id, primaryKey, primaryKeyName)

import Basics.Extra exposing (curry, flip)
import Dict exposing (Dict)
import Dict.Extra as Dict
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
import Schema.Definition exposing (Definition)
import Time.Extra as Time
import Value exposing (Column, Value(..))


type alias Record =
    Dict String Value


encode : Record -> Encode.Value
encode record =
    case primaryKeyName record of
        Just pkName ->
            record
                |> Dict.remove pkName
                |> Encode.dict identity Value.encode

        Nothing ->
            Encode.null


primaryKeyName : Record -> Maybe String
primaryKeyName record =
    Dict.find (\_ v -> Value.isPrimaryKey v) record
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
        |> List.filterMap Value.toPrimaryKey
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
            case Dict.get name definition |> Maybe.map .value of
                Just (PFloat _) ->
                    maybe float |> map PFloat dict

                Just (PInt _) ->
                    maybe int |> map PInt dict

                Just (PString _) ->
                    maybe string |> map PString dict

                Just (PBool _) ->
                    maybe bool |> map PBool dict

                Just (PTime _) ->
                    maybe Time.decoder |> map PTime dict

                Just (PPrimaryKey _) ->
                    maybe PrimaryKey.decoder
                        |> map PPrimaryKey dict

                Just (PForeignKey ( table, col ) _ _) ->
                    let
                        mapFun d pk =
                            insert dict <| PForeignKey ( table, col ) d pk

                        refDec i =
                            Decode.at [ table, i ] (nullable string)
                    in
                    Decode.map2 mapFun
                        (Decode.oneOf <| List.map refDec identifiers)
                        (maybe <| Decode.field name PrimaryKey.decoder)

                Just (BadValue _) ->
                    map BadValue dict Decode.value

                Nothing ->
                    map BadValue dict Decode.value
    in
    Decode.andThen foldFun prevDec
