module Main exposing (main)

import Browser
import PostgrestAdmin exposing (Model, Msg)
import PostgrestAdmin.Config as Config exposing (Config)


main : Program () Model Msg
main =
    Config.default
        |> Config.withUrl "http://localhost:4000"
        |> PostgrestAdmin.application
