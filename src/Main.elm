module Main exposing (main)

import BasicAuth
import Json.Decode as Decode
import PostgrestAdmin exposing (Model, Msg)
import PostgrestAdmin.Config as Config


main : Program Decode.Value Model Msg
main =
    let
        auth =
            BasicAuth.noFlags
                |> BasicAuth.withUrl "http://localhost:4000/rpc/login"
    in
    Config.noFlags
        |> Config.withUrl "http://localhost:4000"
        |> Config.withBasicAuth auth
        |> PostgrestAdmin.application
