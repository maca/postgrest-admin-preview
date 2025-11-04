module Main exposing (main)

import PostgRestAdmin
import PostgRestAdmin.Config as Config
import PostgRestAdmin.Config.FormAuth as FormAuth


main : PostgRestAdmin.Program Never Never Never
main =
    Config.init
        |> Config.host "http://localhost:9080"
        |> Config.formAuth
            (FormAuth.config
                |> FormAuth.authUrl "http://localhost:9080/rpc/login"
            )
        |> PostgRestAdmin.application
