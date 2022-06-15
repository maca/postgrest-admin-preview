module Internal.Download exposing (Download, Format(..), fetch, init, save)

import Bytes exposing (Bytes)
import File.Download as Download
import Http exposing (header)
import Internal.Http exposing (Error(..), handleResponse)
import PostgRestAdmin.Client as Client exposing (Client)
import Task exposing (Task)
import Url


type Format
    = CSV
    | JSON


type Download
    = Pending Format String
    | Complete Format String Bytes


init : Format -> String -> Download
init format_ urlStr =
    Pending format_ urlStr


url : Download -> String
url download =
    case download of
        Pending _ urlStr ->
            urlStr

        Complete _ urlStr _ ->
            urlStr


format : Download -> Format
format download =
    case download of
        Pending format_ _ ->
            format_

        Complete format_ _ _ ->
            format_


fetch : Client -> Download -> Task Error Download
fetch client download =
    let
        host =
            Client.toHostUrl client
    in
    case Client.toJwtString client of
        Just token ->
            Http.task
                { method = "GET"
                , headers =
                    [ header "Bearer" token
                    , case format download of
                        CSV ->
                            header "Accept" "text/csv"

                        JSON ->
                            header "Accept" "application/json"
                    ]
                , url = Url.toString { host | path = url download }
                , body = Http.emptyBody
                , resolver =
                    Http.bytesResolver (handleResponse (\_ body -> Ok body))
                , timeout = Nothing
                }
                |> Task.map (Complete (format download) (url download))

        Nothing ->
            Task.fail AuthError


save : String -> Download -> Cmd msg
save name download =
    case download of
        Complete format_ _ body ->
            case format_ of
                CSV ->
                    Download.bytes (name ++ ".csv") "text/csv" body

                JSON ->
                    Download.bytes (name ++ ".json") "application/json" body

        Pending _ _ ->
            Cmd.none
