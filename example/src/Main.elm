port module Main exposing (main)

import Http
import PostgRestAdmin


port gotToken : String -> Cmd msg


main : PostgRestAdmin.Program Never Never Never
main =
    PostgRestAdmin.application
        [ PostgRestAdmin.host "http://localhost:9080"
        , PostgRestAdmin.clientHeaders
            [ Http.header "Accept-Profile" "bluebox"
            , Http.header "Content-Profile" "bluebox"
            ]
        , PostgRestAdmin.onLogin gotToken
        ]
