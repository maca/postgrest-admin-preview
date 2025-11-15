port module Main exposing (main)

import Http
import PostgRestAdmin
import PostgRestAdmin.Config as Config


port gotToken : String -> Cmd msg


main : PostgRestAdmin.Program Never Never Never
main =
    Config.init
        |> Config.host "http://localhost:9080"
        |> Config.clientHeaders
            [ Http.header "Accept-Profile" "bluebox"
            , Http.header "Content-Profile" "bluebox"
            ]
        |> Config.onLogin gotToken
        |> PostgRestAdmin.application
