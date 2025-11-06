port module Main exposing (main)

import PostgRestAdmin
import PostgRestAdmin.Config as Config
import PostgRestAdmin.Config.FormAuth as FormAuth


port gotToken : String -> Cmd msg


main : PostgRestAdmin.Program Never Never Never
main =
    Config.init
        |> Config.host "http://localhost:9080"
        |> Config.formAuth
            (FormAuth.config
                |> FormAuth.authUrl "http://localhost:9080/rpc/login"
            )
        |> Config.onLogin gotToken
        |> PostgRestAdmin.application
