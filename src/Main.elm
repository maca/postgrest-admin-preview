module Main exposing (main)

import BasicAuth
import PostgrestAdmin
import PostgrestAdmin.Config as Config


type Msg
    = Up
    | Down


type Model
    = Int


main : PostgrestAdmin.Program Model Msg
main =
    Config.init
        |> Config.withBasicAuth BasicAuth.config
        |> PostgrestAdmin.application
