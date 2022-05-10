module Postgrest.Upload exposing (post)

import Http exposing (header)
import Json.Encode exposing (Value)
import Postgrest.Record.Client as Client exposing (Client)
import Task exposing (Task)
import Url
import Utils.Task exposing (Error(..), handleResponse)


post : Client a -> String -> Value -> Task Error ()
post ({ host } as client) path value =
    case Client.jwtString client of
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
