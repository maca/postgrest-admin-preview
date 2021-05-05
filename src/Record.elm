module Record exposing (Record, decoder)

import Dict exposing (Dict)
import Json.Decode as Decode
    exposing
        ( Decoder
        , bool
        , decodeValue
        , float
        , int
        , nullable
        , string
        )
import Postgrest.Client as PG
import PrimaryKey
import Schema exposing (Definition)
import Value exposing (Column, Value(..))


type alias Record =
    Dict String Value


decoder : Definition -> Decoder Record
decoder definition =
    Decode.map (Dict.fromList << List.map (decoderHelp definition))
        (Decode.keyValuePairs Decode.value)


decoderHelp : Definition -> ( String, Decode.Value ) -> ( String, Value )
decoderHelp definition ( name, raw ) =
    let
        map cons dec =
            ( name, decodeValue dec raw |> Result.toMaybe |> cons )
    in
    case Dict.get name definition |> Maybe.map .value of
        Just (PFloat _) ->
            map PFloat float

        Just (PInt _) ->
            map PInt int

        Just (PString _) ->
            map PString string

        Just (PBool _) ->
            map PBool bool

        Just (PForeignKey column _) ->
            map (PForeignKey column) PrimaryKey.decoder

        Just (PPrimaryKey _) ->
            map PPrimaryKey PrimaryKey.decoder

        _ ->
            ( name, BadValue "?" )
