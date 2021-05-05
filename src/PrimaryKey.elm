module PrimaryKey exposing (PrimaryKey(..), decoder, toString)

import Json.Decode as Decode exposing (Decoder, field, int, maybe, string)


type PrimaryKey
    = PrimaryKeyInt Int
    | PrimaryKeyString String


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
