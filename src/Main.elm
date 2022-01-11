module Main exposing (main)

import Browser
import Json.Decode as Decode exposing (Decoder, Value)
import PostgrestAdmin exposing (Model, Msg)
import PostgrestAdmin.Config as Config exposing (Config)


main : Program Decode.Value Model Msg
main =
    Decode.succeed Config.default
        |> Config.withUrl "http://localhost:4000"
        |> Config.withJwt tokenStr
        |> PostgrestAdmin.application


tokenStr : String
tokenStr =
    "eyJhbGciOiJIUzI1NiIsInR5cCI6IkpXVCJ9.eyJyb2xlIjoiYXBwX3VzZXIiLCJlbWFpbCI6ImFkbWluQGV4YW1wbGUuY29tIiwiZXhwIjoxNjQxODc2MDQ1fQ.ROrd1cpcmXr1FhGc6RGAZcSZaQz9uwchNBzZwfUDckk"
