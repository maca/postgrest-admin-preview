module Internal.Download exposing (Download, Format(..), fetch, init, save)

import Bytes exposing (Bytes)
import File.Download as Download
import Http exposing (header)
import PostgRestAdmin.Client as Client exposing (Client, Error, endpoint)
import Task exposing (Task)


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
    case Client.authHeader client of
        Just authHeader ->
            Http.task
                { method = "GET"
                , headers =
                    [ authHeader
                    , case format download of
                        CSV ->
                            header "Accept" "text/csv"

                        JSON ->
                            header "Accept" "application/json"
                    ]
                , url = endpoint client (url download)
                , body = Http.emptyBody
                , resolver =
                    Http.bytesResolver
                        (\response ->
                            case response of
                                Http.BadUrl_ urlStr ->
                                    Err (Client.BadUrl urlStr)

                                Http.Timeout_ ->
                                    Err Client.Timeout

                                Http.BadStatus_ { statusCode } _ ->
                                    case statusCode of
                                        401 ->
                                            Err Client.Unauthorized

                                        403 ->
                                            Err Client.Forbidden

                                        _ ->
                                            Err (Client.BadStatus statusCode)

                                Http.NetworkError_ ->
                                    Err Client.NetworkError

                                Http.GoodStatus_ _ body ->
                                    Ok body
                        )
                , timeout = Nothing
                }
                |> Task.map (Complete (format download) (url download))

        Nothing ->
            Task.fail Client.Unauthorized


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
