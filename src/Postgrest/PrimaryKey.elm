module Postgrest.PrimaryKey exposing (PrimaryKey(..), decoder, encode, toString)

import Json.Decode as Decode exposing (Decoder, int, string)
import Json.Encode as Encode


type PrimaryKey
    = PrimaryKeyInt Int
    | PrimaryKeyString String


encode : PrimaryKey -> Encode.Value
encode pk =
    case pk of
        PrimaryKeyInt int ->
            Encode.int int

        PrimaryKeyString string ->
            Encode.string string


decoder : Decoder PrimaryKey
decoder =
    Decode.oneOf
        [ Decode.map PrimaryKeyInt int
        , Decode.map PrimaryKeyString string
        ]


toString : PrimaryKey -> String
toString primaryKey =
    case primaryKey of
        PrimaryKeyInt int ->
            String.fromInt int

        PrimaryKeyString string ->
            string
