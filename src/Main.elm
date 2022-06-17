module Main exposing (main)

import Browser.Navigation as Nav
import PostgRestAdmin
import PostgRestAdmin.Config as Config
import PostgRestAdmin.Config.FormAuth as FormAuth


main : PostgRestAdmin.Program Never Never
main =
    Config.init
        |> Config.withFormAuth FormAuth.config
        |> PostgRestAdmin.application
