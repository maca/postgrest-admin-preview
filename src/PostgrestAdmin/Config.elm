module PostgrestAdmin.Config exposing (Config, default, url, withUrl)

import Result
import Url exposing (Protocol(..), Url)


type alias Config =
    { url : Url
    }


default : Config
default =
    { url =
        { protocol = Http
        , host = "localhost"
        , port_ = Just 3000
        , path = ""
        , query = Nothing
        , fragment = Nothing
        }
    }


withUrl : String -> Config -> Result String Config
withUrl urlStr config =
    Url.fromString urlStr
        |> Maybe.map (\u -> Ok { config | url = u })
        |> Maybe.withDefault (Err "Postgrest url is not valid")


url : Config -> String
url config =
    Url.toString config.url
