port module Main exposing (main)

import PostgRestAdmin
import PostgRestAdmin.Config as Config
import PostgRestAdmin.Config.FormAuth as FormAuth


port authFailure : String -> Cmd msg


port tokenReceiver : ({ path : String, accessToken : String } -> msg) -> Sub msg


port logout : () -> Cmd msg


main : PostgRestAdmin.Program Never Never
main =
    Config.init
        |> Config.withOnAuthFailed authFailure
        |> Config.withOnExternalLogin tokenReceiver
        |> Config.withOnLogout logout
        |> PostgRestAdmin.application
