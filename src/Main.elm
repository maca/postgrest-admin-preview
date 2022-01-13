module Main exposing (main)

import Json.Decode as Decode
import PostgrestAdmin exposing (Model, Msg)
import PostgrestAdmin.Config as Config


main : Program Decode.Value Model Msg
main =
    Config.noFlags
        |> Config.withUrl "http://localhost:4000"
        |> Config.withJwt tokenStr
        |> PostgrestAdmin.application


tokenStr : String
tokenStr =
    "eyJhbGciOiJIUzI1NiIsInR5cCI6IkpXVCJ9.eyJyb2xlIjoiYXBwX3VzZXIiLCJlbWFpbCI6ImFkbWluQGV4YW1wbGUuY29tIiwiZXhwIjoxNjQxOTQ1NTU3fQ.ntpyIrsVYBB7lacORt318d8V7wuqYf2228rIfRFYWk4"
