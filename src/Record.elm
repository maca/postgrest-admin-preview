module Record exposing (Record, decoder)

import Basics.Extra exposing (curry, flip)
import Dict exposing (Dict)
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
import Postgrest.Client as PG
import PrimaryKey
import Schema exposing (Definition)
import Value exposing (Column, Value(..))


type alias Record =
    Dict String Value


decoder : List String -> Definition -> Decoder Record
decoder identifiers definition =
    definition
        |> Dict.foldl (decoderFold identifiers definition)
            (Decode.succeed Dict.empty)


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
                    nullable float |> map PFloat dict

                Just (PInt _) ->
                    nullable int |> map PInt dict

                Just (PString _) ->
                    nullable string |> map PString dict

                Just (PBool _) ->
                    nullable bool |> map PBool dict

                Just (PPrimaryKey _) ->
                    nullable PrimaryKey.decoder
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
                        (Decode.field name (nullable PrimaryKey.decoder))

                _ ->
                    map BadValue dict Decode.value
    in
    Decode.andThen foldFun prevDec
