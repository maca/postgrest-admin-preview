module Internal.Upload exposing (post)

import Http exposing (header)
import Json.Encode exposing (Value)
import PostgRestAdmin.Client as Client exposing (Client)
import Task exposing (Task)
import Url
import Utils.Task exposing (Error(..), handleResponse)


post : Client -> String -> Value -> Task Error ()
post client path value =
    let
        host =
            Client.toHostUrl client
    in
    case Client.toJwtString client of
        Just token ->
            Http.task
                { method = "POST"
                , headers =
                    [ header "Authorization" ("Bearer " ++ token)
                    , header "Prefer" "resolution=merge-duplicates"
                    ]
                , url = Url.toString { host | path = path }
                , body = Http.jsonBody value
                , resolver = Http.stringResolver (handleResponse (\_ -> Ok ()))
                , timeout = Nothing
                }

        Nothing ->
            Task.fail AuthError
