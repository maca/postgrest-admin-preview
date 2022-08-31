port module Main exposing (main)

import PostgRestAdmin
import PostgRestAdmin.Config as Config
import PostgRestAdmin.Config.FormAuth as FormAuth


port authFailure : String -> Cmd msg


port logout : () -> Cmd msg


port tokenReceiver : ({ path : String, accessToken : String } -> msg) -> Sub msg


main : PostgRestAdmin.Program Never Never
main =
    Config.init
        |> Config.onAuthFailed authFailure
        |> Config.onLogout logout
        |> Config.subscribeToExternalLogin tokenReceiver
        |> PostgRestAdmin.application
