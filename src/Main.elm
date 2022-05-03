module Main exposing (main)

import BasicAuth
import Json.Decode as Decode
import PostgrestAdmin exposing (Model, Msg)
import PostgrestAdmin.Config as Config


main : Program Decode.Value Model Msg
main =
    Config.init
        |> Config.withBasicAuth BasicAuth.config
        |> PostgrestAdmin.application
